{-# LANGUAGE OverloadedStrings #-}

module View.SolarSystemView
  ( solarSystemToJSON
  ) where

import Model.SolarSystem
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- Instância ToJSON customizada
instance ToJSON SolarSystem where
  toJSON (SolarSystem sid name race economy conflict note galaxyId) =
    object
      [ "id"        .= sid
      , "name"      .= name
      , "race"      .= race
      , "economy"   .= economy
      , "conflict"  .= conflict
      , "note"      .= note
      , "galaxy_id" .= galaxyId
      ]

-- Função auxiliar (igual ao GalaxyView)
solarSystemToJSON :: SolarSystem -> SolarSystem
solarSystemToJSON = id
