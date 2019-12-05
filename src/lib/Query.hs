{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Query
  ( getElementByID, getElementsByUser, addElement, deleteElement, triggerTimer
  , createNewUser, getUserBySession
  )
where

import Data.Int
import Data.Text ( Text )
import Data.Time
import Data.Maybe ( listToMaybe )

import Text.RawString.QQ ( r )

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Types
import DBConversion ()

-- We choose to store the elements in the database as separate tables
-- for `timers` and `notes`, with a single `elements` table that stores
-- the user ID and element type. When queyring this, we need to manually
-- join the data together, one row for each element.

data RawElementData = RawElementData
  { rawElemID :: Int32
  , rawElemUserID :: Int32
  , rawElemType :: Text
  , rawTimerName :: Maybe Text
  , rawTimerDuration :: Maybe Int64  -- ^ In seconds.
  , rawTimerTriggeredTime :: Maybe UTCTime
  , rawNoteText :: Maybe Text
  }
  deriving (Eq, Show)

instance FromRow RawElementData where
  fromRow = RawElementData <$> field <*> field <*> field <*> field <*> field <*> field <*> field

convertRawElement :: RawElementData -> Maybe Elem
convertRawElement dat = do
  elemData <- case rawElemType dat of
    "timer" -> do
      duration <- rawTimerDuration dat
      pure $ TimerElem $ Timer
        { timerName = rawTimerName dat
        , timerDuration = fromIntegral duration
        , timerTriggeredTime = rawTimerTriggeredTime dat
        }
    "note" -> do
      text <- rawNoteText dat
      pure $ NoteElem $ Note
        { noteText = text }
    _ -> Nothing
  pure $ Elem
    { elemID = ElemID (rawElemID dat)
    , elemUserID = UserID (rawElemUserID dat)
    , elemData = elemData
    }

getElementByID :: Connection -> UserID -> ElemID -> IO (Maybe Elem)
getElementByID conn uid eid =
  fmap (>>= convertRawElement) $ getRawElementByID conn uid eid

-- We propagate a Maybe [Elem] here, just in case our query returns invalid
-- results. We have the endpoint itself handle producing a valid error
-- message.
getElementsByUser :: Connection -> UserID -> IO (Maybe [Elem])
getElementsByUser conn uid =
  fmap (mapM convertRawElement) $ getRawElementsByUser conn uid

getRawElementByID :: Connection -> UserID -> ElemID -> IO (Maybe RawElementData)
getRawElementByID conn uid eid =
  fmap listToMaybe $
    flip (query conn) (eid, uid) $
      [r|
        SELECT e.id, e.user_id, e.elem_type, t.name, t.duration, t.triggered_time, n.text
          FROM timers t
          FULL OUTER JOIN notes n
            ON false
         INNER JOIN elements e
            ON t.id = e.id OR n.id = e.id
         WHERE e.id = ?
           AND e.user_id = ?
         ORDER BY e.id ASC;
      |]

getRawElementsByUser :: Connection -> UserID -> IO [RawElementData]
getRawElementsByUser conn uid =
  flip (query conn) (Only uid) $
    [r|
      SELECT e.id, e.user_id, e.elem_type, t.name, t.duration, t.triggered_time, n.text
        FROM timers t
        FULL OUTER JOIN notes n
          ON false
       INNER JOIN elements e
          ON t.id = e.id OR n.id = e.id
       WHERE e.user_id = ?
       ORDER BY e.id ASC;
    |]

getUserBySession :: Connection -> SessionID -> IO (Maybe User)
getUserBySession conn sid =
  fmap listToMaybe $
    flip (query conn) (Only sid) $
      [r|
        SELECT u.id, u.session_id
          FROM users as u
         WHERE u.session_id = ?;
      |]

createNewUser :: Connection -> SessionID -> IO (Maybe User)
createNewUser conn sid =
  fmap listToMaybe $
    flip (query conn) (Only sid) $
      [r|
        INSERT INTO users (id, session_id)
        VALUES (DEFAULT, ?)
        RETURNING id, session_id;
      |]

addElement :: Connection -> UserID -> ElemData -> IO (Maybe ElemID)
addElement conn uid payload = do
  let elemType = case payload of
        TimerElem _ -> "timer" :: Text
        NoteElem _ -> "note" :: Text
  meid <- fmap (fmap ElemID . fmap fromOnly . listToMaybe) $
    flip (query conn) (uid, elemType) $
      [r|
        INSERT INTO elements (id, user_id, elem_type)
        VALUES (DEFAULT, ?, ?)
        RETURNING id;
      |]
  case meid of
    Nothing -> pure ()
    Just eid -> case payload of
      TimerElem timer -> do
        _ <- flip (execute conn) (toField eid : toRow timer) $
          [r|
            INSERT INTO timers (id, name, duration, triggered_time)
            VALUES (?, ?, ?, ?);
          |]
        pure ()
      NoteElem note -> do
        _ <- flip (execute conn) (toField eid : toRow note) $
          [r|
            INSERT INTO notes (id, text)
            VALUES (?, ?);
          |]
        pure ()
  pure meid

deleteElement :: Connection -> ElemID -> IO ()
deleteElement conn eid = do
  _ <- flip (execute conn) (Only eid) "DELETE FROM timers WHERE id = ?;"
  _ <- flip (execute conn) (Only eid) "DELETE FROM notes WHERE id = ?;"
  _ <- flip (execute conn) (Only eid) "DELETE FROM elements WHERE id = ?;"
  pure ()

triggerTimer :: Connection -> ElemID -> IO (Maybe Timer)
triggerTimer conn eid =
  fmap listToMaybe $
    flip (query conn) (Only eid) $
      [r|
        UPDATE timers
           SET triggered_time = CURRENT_TIMESTAMP
         WHERE id = ?
        RETURNING name, duration, triggered_time;
      |]
