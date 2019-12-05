{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Rank2Types #-}

module AppHandlers
  ( indexHandler, dashboardHandler, jsHandler
  , getElementsHandler, createElementHandler, deleteElementHandler
  , triggerTimerHandler
  , dashboardHTML
  )
where

import Prelude hiding ( log )

import Data.Int ( Int32 )
import Data.Text ( Text )
import Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

import Control.Monad.IO.Class

import qualified Database.PostgreSQL.Simple as PG

import qualified System.Log.FastLogger as Log

import qualified Text.Blaze.Html5 as HTML
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as HTML hiding ( title )
import qualified Text.Blaze.Html.Renderer.Utf8 as HTML

import Network.HTTP.Types.Status as HTTP

import Web.Spock as Spock

import Types
import Query
import JSON ()

type Handler a = forall ctx sess. ActionCtxT ctx (WebStateM PG.Connection sess Log.LoggerSet) a

--------------------
-- HANDLERS
--------------------

indexHandler :: Handler ()
indexHandler = do
  sid <- fmap SessionID getSessionId
  user <- loadOrCreateUser sid
  let UserID uid = userID user
  redirect ("dashboard/" <> T.pack (show uid))

dashboardHandler :: Int32 -> Handler ()
dashboardHandler _uid = do
  let uid = UserID _uid
  sid <- fmap SessionID getSessionId
  muser <- loadUser sid
  if fmap userID muser == Just uid
    then do
      setHeader "Content-Type" "text/html"
      lazyBytes $ HTML.renderHtml dashboardHTML
    else do
      setStatus HTTP.unauthorized401
      html "<h2>You can't view someone else's dashboard.</h2>"

jsHandler :: Text -> Handler ()
jsHandler path =
  file "application/javascript" ("js/" <> T.unpack path)

getElementsHandler :: Int32 -> Handler ()
getElementsHandler _uid = do
  let uid = UserID _uid
  authAPIAs uid

  log ("getting elements for user " <> T.pack (show _uid))

  elements <- runQuery (\conn -> getElementsByUser conn uid)
  case elements of
    Nothing ->
      jsonError HTTP.internalServerError500 "failed-fetching-elements"
    Just elements ->
      jsonOK [ "user_id" .= _uid, "elements" .= elements ]

createElementHandler :: Int32 -> Handler ()
createElementHandler _uid = do
  let uid = UserID _uid
  authAPIAs uid

  log ("creating new element for user " <> T.pack (show _uid))

  elemJSON <- jsonBodyWith' onErr $
    withObject "Elem" $ \obj -> do
      wrapper <- obj .: "element"
      parseJSON wrapper
  meid <- runQuery (\conn -> addElement conn uid elemJSON)
  case meid of
    Nothing ->
      jsonError HTTP.internalServerError500 "failed-inserting-element"
    Just eid ->
      let ElemID _eid = eid
      in jsonOK [ "element_id" .= _eid ]

deleteElementHandler :: Int32 -> Int32 -> Handler ()
deleteElementHandler _uid _eid = do
  let uid = UserID _uid
      eid = ElemID _eid
  authAPIAs uid

  log ("deleting element " <> T.pack (show _eid) <> " for user " <> T.pack (show _uid))

  runQuery (\conn -> deleteElement conn eid)
  jsonOK [ "element_id" .= _eid ]

triggerTimerHandler :: Int32 -> Int32 -> Handler ()
triggerTimerHandler _uid _eid = do
  let uid = UserID _uid
      eid = ElemID _eid
  authAPIAs uid

  log ("triggering timer " <> T.pack (show _eid) <> " for user " <> T.pack (show _uid))

  melem <- runQuery (\conn -> getElementByID conn uid eid)
  case melem of
    Nothing ->
      jsonError HTTP.notFound404 "element-not-found"
    Just elem -> case elemData elem of
      NoteElem _ ->
        jsonError HTTP.methodNotAllowed405 "element-not-timer"
      TimerElem _ -> do
        mtimer <- runQuery (\conn -> triggerTimer conn eid)
        case mtimer of
          Nothing ->
            jsonError HTTP.internalServerError500 "failed-trigger-timer"
          Just timer ->
            jsonOK
              [ "element_id" .= _eid
              , "timer_duration" .= timerDuration timer
              , "timer_triggered_time" .= timerTriggeredTime timer
              ]

--------------------
-- HTML
--------------------

dashboardHTML :: HTML.Html
dashboardHTML = HTML.html $
  HTML.docTypeHtml $ do
    HTML.head $ do
      HTML.title "Timers and Notes"
      HTML.meta ! HTML.charset "utf-8"
      HTML.script ! HTML.src "/js/bundle.js" $ ""
    HTML.body $ do
      HTML.div ! HTML.id "content" $ ""

--------------------
-- API HELPERS
--------------------

-- Since it's specified that we always return JSON with a outer 'data' wrapper
-- property, we define some helpers here to make dealing with that less verbose.

wrapData :: ToJSON a => a -> Value
wrapData inner = object [ "data" .= inner ]

jsonOK :: MonadIO m => Object -> ActionCtxT ctx m b
jsonOK obj =
  let withOK = HM.insert "ok" (Bool True) obj
  in Spock.json $ wrapData withOK

jsonError :: MonadIO m => HTTP.Status -> Text -> ActionCtxT ctx m a
jsonError status text = do
  setStatus status
  Spock.json $ wrapData $ object
    [ "ok" .= False
    , "error" .= text
    ]

onErr :: MonadIO m => String -> ActionCtxT ctx m a
onErr err = jsonError HTTP.badRequest400 (T.pack err)

-- |
-- Check if the authentication info in the current request matches the given
-- user ID. Send back a JSON error if not.
authAPIAs :: UserID -> Handler ()
authAPIAs uid = authAs uid (jsonError HTTP.unauthorized401 "unauthorized")

log :: Text -> Handler ()
log msg = do
  loggerSet <- Spock.getState
  liftIO $ Log.pushLogStrLn loggerSet (Log.toLogStr msg)

--------------------
-- AUTH AND PARSING
--------------------

-- |
-- Parse the body of the request as JSON using the given parser, instead of
-- using `FromJSON' instances. Call the given error handler with the failure
-- message on parse failure. The error handler should call something that
-- halts processing of the current request, like `Spock.json' or `Spock.html'.
jsonBodyWith' :: MonadIO m
              => (String -> ActionCtxT ctx m a)
              -> (Value -> Parser a)
              -> ActionCtxT ctx m a
jsonBodyWith' onErr parser = do
  b <- body
  case eitherDecodeStrict' b >>= parseEither parser of
    Left err -> onErr err
    Right result -> pure result

-- |
-- Check if the authentication information in the current request matches
-- the given user ID. Call the provided error handler if the user doesn't
-- match. The error handler should call something that halts processing
-- of the current request, like `Spock.json' or `Spock.html'.
authAs :: UserID -> Handler () -> Handler ()
authAs uid onAuthErr = do
  mauthToken <- cookie "SESSION_ID"
  case mauthToken of
    Nothing -> onAuthErr
    Just token -> do
      muser <- loadUser (SessionID token)
      if (fmap userID muser /= Just uid)
        then onAuthErr
        else pure ()

-- |
-- Get the user associated with the provided authentication information.
loadUser :: SessionID -> Handler (Maybe User)
loadUser authToken = runQuery (\conn -> getUserBySession conn authToken)

-- |
-- Get the user associated with the provided authentication information, or
-- create one if none exists.
loadOrCreateUser :: SessionID -> Handler User
loadOrCreateUser authToken = do
  muser <- loadUser authToken
  case muser of
    Just user -> pure user
    Nothing -> do
      muser <- runQuery (\conn -> createNewUser conn authToken)
      case muser of
        Just user -> pure user
        Nothing -> error "couldn't create user"
