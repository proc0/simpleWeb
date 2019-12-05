{-# LANGUAGE OverloadedStrings #-}

module DBConversion
  ()
where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Types

mapParser :: (a -> b) -> (FieldParser a -> FieldParser b)
mapParser = fmap . fmap . fmap

instance ToField UserID where
  toField (UserID id) = toField id
instance FromField UserID where
  fromField = mapParser UserID fromField

instance ToField ElemID where
  toField (ElemID id) = toField id
instance FromField ElemID where
  fromField = mapParser ElemID fromField

instance ToField SessionID where
  toField (SessionID id) = toField id
instance FromField SessionID where
  fromField = mapParser SessionID fromField

instance ToRow User where
  toRow user = [toField (userID user), toField (userSessionID user)]
instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Timer where
  fromRow = Timer <$> field <*> (fmap fromInteger field) <*> field
instance ToRow Timer where
  toRow timer =
    [ toField (timerName timer)
    , toField (truncate $ timerDuration timer :: Integer)
    , toField (timerTriggeredTime timer)
    ]

instance ToRow Note where
  toRow note = [ toField (noteText note) ]
