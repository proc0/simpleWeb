module Types
  ( UserID(..), ElemID(..), SessionID(..)
  , User(..), Elem(..), ElemData(..), Timer(..), Note(..)
  )
where

import Data.Int
import Data.Text ( Text )
import Data.Time

newtype UserID = UserID Int32 deriving (Eq, Show)
newtype ElemID = ElemID Int32 deriving (Eq, Show)
newtype SessionID = SessionID Text deriving (Eq, Show)

data User = User
  { userID :: UserID
  , userSessionID :: SessionID
  }
  deriving (Eq, Show)

data Elem = Elem
  { elemID :: ElemID
  , elemUserID :: UserID
  , elemData :: ElemData
  }
  deriving (Eq, Show)

data ElemData
  = TimerElem Timer
  | NoteElem Note
  deriving (Eq, Show)

data Timer = Timer
  { timerName :: Maybe Text
  , timerDuration :: NominalDiffTime
  , timerTriggeredTime :: Maybe UTCTime
  }
  deriving (Eq, Show)

data Note = Note
  { noteText :: Text }
  deriving (Eq, Show)
