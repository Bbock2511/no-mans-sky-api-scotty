{-# LANGUAGE OverloadedStrings #-}

module Model.Msg
  ( Msg(..)
  , insertMsg
  , getMsgs
  , getMsgById
  , updateMsg
  , deleteMsg
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow()
import Data.Text (Text)

data Msg = Msg
  { msgId :: Int
  , msgText :: Text
  } deriving (Show, Eq)

instance FromRow Msg where
  fromRow = Msg <$> field <*> field

instance ToRow Msg where
  toRow (Msg id text) = toRow (id, text)

insertMsg :: Connection -> Text -> IO ()
insertMsg conn txt = execute conn "INSERT INTO msgs (text) VALUES (?)" (Only txt)

getMsgs :: Connection -> IO [Msg]
getMsgs conn = query_ conn "SELECT * FROM msgs"

getMsgById :: Connection -> Int -> IO (Maybe Msg)
getMsgById conn mid = do
  results <- query conn "SELECT * FROM msgs WHERE id = ?" (Only mid)
  return $ case results of
    [msg] -> Just msg
    _     -> Nothing

updateMsg :: Connection -> Int -> Text -> IO ()
updateMsg conn mid newText =
  execute conn "UPDATE msgs SET text = ? WHERE id = ?" (newText, mid)

deleteMsg :: Connection -> Int -> IO ()
deleteMsg conn mid = execute conn "DELETE FROM msgs WHERE id = ?" (Only mid)
