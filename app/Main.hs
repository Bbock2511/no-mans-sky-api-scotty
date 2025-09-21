{-# LANGUAGE OverloadedStrings #-}


module Main where


import Web.Scotty
import Database.SQLite.Simple
import Network.HTTP.Types.Status (status200)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL


main :: IO ()
main = do
    -- cria/abre banco SQLite local (db.sqlite no workspace)
    conn <- open "db.sqlite"
    execute_ conn "CREATE TABLE IF NOT EXISTS msgs (id INTEGER PRIMARY KEY AUTOINCREMENT, text TEXT)"
    close conn

    scotty 3000 $ do
        get "/" $ do
            text "Hello from Scotty!"

        get "/ping" $ do
            status status200
            json ("pong" :: Text)


        post "/echo" $ do
            b <- body
            text (TL.fromStrict (decodeUtf8 b))