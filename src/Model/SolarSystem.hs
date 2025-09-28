{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.SolarSystem
  ( SolarSystem(..)
  , SolarSystemInsert(..)
  , insertSolarSystem
  , getSolarSystems
  , getSolarSystemByID
  , updateSolarSystem
  , deleteSolarSystem
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow()
import Database.SQLite.Simple.ToRow()
import GHC.Generics
import Data.Text (Text)

data SolarSystem = SolarSystem
    { systemId       :: Int
    , systemName     :: Text
    , systemRace     :: Text
    , systemEconomy  :: Text
    , systemConflict :: Text
    , systemNotes    :: Maybe Text
    , galaxyIdFk     :: Int   -- chave estrangeira para Galaxy
    } deriving (Show, Generic, Eq)

-- Para SELECTs
instance FromRow SolarSystem where
  fromRow = SolarSystem <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Para INSERTs
data SolarSystemInsert = SolarSystemInsert
    { siName      :: Text
    , siRace      :: Text
    , siEconomy   :: Text
    , siConflict  :: Text
    , siNotes     :: Maybe Text
    , siGalaxyId  :: Int
    } deriving (Show, Generic, Eq)

instance ToRow SolarSystemInsert where
  toRow (SolarSystemInsert n r e c sn gid) = toRow (n, r, e, c, sn, gid)

-- Insert
insertSolarSystem :: Connection -> SolarSystemInsert -> IO ()
insertSolarSystem conn s =
    execute conn
      "INSERT INTO solar_systems (name, race, economy, conflict, note, galaxy_id) VALUES (?, ?, ?, ?, ?, ?)"
      s

-- Select *
getSolarSystems :: Connection -> IO [SolarSystem]
getSolarSystems conn =
    query_ conn "SELECT id, name, race, economy, conflict, note, galaxy_id FROM solar_systems"

-- Select by id
getSolarSystemByID :: Connection -> Int -> IO (Maybe SolarSystem)
getSolarSystemByID conn ssid = do
    result <- query conn
        "SELECT * FROM solar_systems WHERE id = ?" (Only ssid)
    return $ case result of
        [solarSystem] -> Just solarSystem
        _             -> Nothing

-- Update
updateSolarSystem :: Connection -> Int -> SolarSystemInsert -> IO ()
updateSolarSystem conn ssid ss = 
    execute conn
        "UPDATE solar_systems SET name = ?, race = ?, economy = ?, conflict = ?, note = ?, galaxy_id = ? WHERE id = ?"
        (siName ss, siRace ss, siEconomy ss, siConflict ss, siNotes ss, siGalaxyId ss, ssid)

-- Delete
deleteSolarSystem :: Connection -> Int -> IO ()
deleteSolarSystem conn ssid =
    execute conn "DELETE FROM solar_systems WHERE id = ?" (Only ssid)
