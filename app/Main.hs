{-# LANGUAGE OverloadedStrings #-}


module Main where

import Web.Scotty
import Model.Msg
import Controller.MsgController
import Database.SQLite.Simple
import Network.HTTP.Types.Status (status200)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TLE

main :: IO ()
main = do 
    -- cria/abre banco SQLite local (db.sqlite no workspace)
    conn <- open "db.sqlite"
    initMsgDB conn

    scotty 3000 $ do
        msgRoutes conn
        get "/" $ do
            text "Welcome to no mans sky api"

        get "/ping" $ do
            status status200
            json ("pong" :: Text)

        post "/echo" $ do
            b <- body  -- ByteString Lazy
            text (TLE.decodeUtf8 b)  -- já Text Lazy

        