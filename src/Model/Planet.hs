{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Planet
    ( Planet(..)
    , PlanetInsert(..)
    , insertPlanet
    , getPlanets
    , getPlanetById
    , updatePlanet
    , deletePlanet
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Text (Text)

data Planet = Planet
    { planetId :: Int
    , planetName :: Text
    , planetWeather :: Text
    , planetSentinels :: Text
    , planetFlora :: Text
    , planetFauna :: Text
    , planetResources :: Text
    , planetNotes :: Maybe Text
    , solarSystemIdFK :: Int    -- Chave estrangeira para SolarSystem
    } deriving (Show, Generic)

instance FromRow Planet where
    fromRow = Planet <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Para inserção
data PlanetInsert = PlanetInsert
    { planetInsertName :: Text
    , planetInsertWeather :: Text
    , planetInsertSentinels :: Text
    , planetInsertFlora :: Text
    , planetInsertFauna :: Text
    , planetInsertResources :: Text
    , planetInsertNotes :: Maybe Text
    , planetInsertSolarSystemId :: Int
    } deriving (Show, Generic)

instance ToRow PlanetInsert where
    toRow (PlanetInsert n w s fl fa r pn ssid) = toRow (n, w, s, fl, fa, r, pn, ssid)

instance FromJSON PlanetInsert
instance ToJSON PlanetInsert

-- Insert
insertPlanet :: Connection -> PlanetInsert -> IO ()
insertPlanet conn p = 
    execute conn "INSERT INTO planets (name, weather, flora, fauna, resources, notes, solar_system_id) VALUES (?, ?, ?, ?, ?, ?, ?)"
    p

-- Select *
getPlanets :: Connection -> IO [Planet]
getPlanets conn =
    query_ conn "SELECT * FROM planets"

-- Select by id
getPlanetById :: Connection -> Int -> IO (Maybe Planet)
getPlanetById conn pid = do
    result <- query conn "SELECT * FROM planets WHERE id = ?" (Only pid)
    return $ case result of
        [planet] -> Just planet
        _        -> Nothing

-- Update
updatePlanet :: Connection -> Int -> PlanetInsert -> IO ()
updatePlanet conn pid p = 
    execute conn 
        "UPDATE planets SET name = ?, weather = ?, sentinels = ?, flora = ?, fauna = ?, resources = ?, notes = ?, solar_system_id = ? WHERE id = ?"
        ( planetInsertName p
        , planetInsertWeather p
        , planetInsertSentinels p
        , planetInsertFlora p
        , planetInsertFauna p
        , planetInsertResources p
        , planetInsertNotes p
        , planetInsertSolarSystemId p
        , pid
        )
    
-- Delete
deletePlanet :: Connection -> Int -> IO ()
deletePlanet conn pid = 
    execute conn "DELETE FROM planets WHERE id = ?" (Only pid)
    