{-# LANGUAGE OverloadedStrings #-}

module View.MsgView
  ( msgToJSON
  ) where

import Model.Msg
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Text.Lazy (Text)

instance ToJSON Msg where
  toJSON (Msg mid txt) =
    object ["id" .= mid, "text" .= txt]

-- função auxiliar, se quiser formatar ou manipular antes de enviar
msgToJSON :: Msg -> Msg
msgToJSON = id  -- aqui você poderia limpar ou processar campos
