{-# LANGUAGE OverloadedStrings #-}

module View.GalaxyView
  ( galaxyToJSON
  ) where

import Model.Galaxy
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- Instância ToJSON customizada
instance ToJSON Galaxy where
  toJSON (Galaxy gid name gtype note) =
    object
      [ "id"   .= gid
      , "name" .= name
      , "type" .= gtype
      , "note" .= note
      ]

-- função auxiliar, se quiser formatar ou manipular antes de enviar
galaxyToJSON :: Galaxy -> Galaxy
galaxyToJSON = id  -- aqui você poderia limpar ou processar campos