{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSON
  ()
where

import Data.Text ( Text )
import Data.Aeson

import Types

instance ToJSON Elem where
  toJSON elem =
    let Elem { elemID = ElemID eid, elemData = elemData } = elem
    in case elemData of
      TimerElem timer -> object
        [ "element_id" .= eid
        , "element_type" .= ("timer" :: Text)
        , "timer_name" .= timerName timer
        , "timer_duration" .= timerDuration timer
        , "timer_triggered_time" .= timerTriggeredTime timer
        ]
      NoteElem note -> object
        [ "element_id" .= eid
        , "element_type" .= ("note" :: Text)
        , "note_text" .= noteText note
        ]

instance FromJSON ElemData where
  parseJSON = withObject "ElemData" $ \obj -> do
    etype :: Text <- obj .: "element_type"
    case etype of
      "timer" -> fmap TimerElem (Timer
        <$> obj .: "timer_name"
        <*> obj .: "timer_duration"
        <*> obj .: "timer_triggered_time")
      "note" -> NoteElem . Note <$> obj .: "note_text"
      _ -> fail "unknown element type"
