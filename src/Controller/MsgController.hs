{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.MsgController
  ( msgRoutes
  ) where

import Web.Scotty
import Model.Msg
import View.MsgView
import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status404)
import Data.Text (Text)
import GHC.Generics (Generic)

  -- Define o tipo que vai receber do JSON
data MsgInput = MsgInput
    { inputText :: Text
    } deriving (Show, Generic)

instance FromJSON MsgInput
instance ToJSON MsgInput

msgRoutes :: Connection -> ScottyM ()
msgRoutes conn = do
  -- GET /msgs
  get "/msgs" $ do
    msgs <- liftIO $ getMsgs conn
    json (map msgToJSON msgs)

  -- GET /msgs/:id
  get "/msgs/:id" $ do
    mid <- captureParam "id"
    msg <- liftIO $ getMsgById conn mid
    case msg of
      Just m  -> json (msgToJSON m)
      Nothing -> status status404 >> text "Mensagem não encontrada"

  -- POST /msgs
  post "/msgs" $ do
    -- lê JSON do corpo da requisição
    msgInput <- jsonData :: ActionM MsgInput
    liftIO $ insertMsg conn (inputText msgInput)
    json msgInput

  -- PUT /msgs/:id
  put "/msgs/:id" $ do
    mid <- captureParam "id"
    msgInput <- jsonData :: ActionM MsgInput
    liftIO $ updateMsg conn mid (inputText msgInput)
    json msgInput

  -- DELETE /msgs/:id
  delete "/msgs/:id" $ do
    mid <- captureParam "id"
    liftIO $ deleteMsg conn mid
    text "Mensagem deletada"
