{-# LANGUAGE OverloadedStrings #-}

module View.PlanetView
  ( planetToJSON
  ) where

import Model.Planet
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- Instância ToJSON customizada
instance ToJSON Planet where
    toJSON (Planet pid name weather sentinels flora fauna resources notes solarSystemId) = 
        object
            [ "id"              .= pid
            , "name"            .= name
            , "weather"         .= weather
            , "sentinels"       .= sentinels
            , "flora"           .= flora
            , "fauna"           .= fauna
            , "resources"       .= resources
            , "notes"           .= notes
            , "solar_system_id" .= solarSystemId
            ]
    
planetToJSON :: Planet -> Planet
planetToJSON = id