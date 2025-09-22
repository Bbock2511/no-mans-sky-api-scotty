{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Galaxy
  ( Galaxy(..)
  , GalaxyInsert(..)
  , initGalaxyDB
  , insertGalaxy
  , getGalaxies
  , getGalaxyById
  , updateGalaxy
  , deleteGalaxy
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow ()
import Data.Text (Text)
import GHC.Generics (Generic)

-- Definição do modelo
data Galaxy = Galaxy
    { galaxyId   :: Int
    , galaxyName :: Text
    , galaxyType :: Text
    , galaxyNote :: Maybe Text
    } deriving (Show, Generic)

-- Tipo para inserir no banco (sem ID)
data GalaxyInsert = GalaxyInsert
    { giName :: Text
    , giType :: Text
    , giNote :: Maybe Text
    } deriving (Show, Generic)

instance FromRow Galaxy where 
    fromRow = Galaxy <$> field <*> field <*> field <*> field

instance ToRow GalaxyInsert where
    toRow (GalaxyInsert name gtype note) = toRow (name, gtype, note)

-- Criação da tabela
initGalaxyDB :: Connection -> IO ()
initGalaxyDB conn =
    execute_ conn
      "CREATE TABLE IF NOT EXISTS galaxies \
      \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
      \name TEXT NOT NULL, \
      \galaxyType TEXT NOT NULL, \
      \note TEXT NULL)"

-- Insert
insertGalaxy :: Connection -> GalaxyInsert -> IO ()
insertGalaxy conn g =
    execute conn
      "INSERT INTO galaxies (name, galaxyType, note) VALUES (?, ?, ?)"
      g

-- Select *
getGalaxies :: Connection -> IO [Galaxy]
getGalaxies conn = query_ conn "SELECT * FROM galaxies"

-- Select by id
getGalaxyById :: Connection -> Int -> IO (Maybe Galaxy)
getGalaxyById conn gid = do 
    result <- query conn
        "SELECT * FROM galaxies WHERE id = ?" (Only gid)
    return $ case result of
        [galaxy] -> Just galaxy
        _        -> Nothing

-- Update
updateGalaxy :: Connection -> Int -> GalaxyInsert -> IO ()
updateGalaxy conn gid g = 
    execute conn
      "UPDATE galaxies SET name = ?, galaxyType = ?, note = ? WHERE id = ?"
      (giName g, giType g, giNote g, gid)

-- Delete
deleteGalaxy :: Connection -> Int -> IO ()
deleteGalaxy conn gid =
    execute conn "DELETE FROM galaxies WHERE id = ?" (Only gid)
