{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

data Galaxy = Galaxy
    { id :: Int
    , name :: Text
    , type :: Text
    , note :: Maybe Text
    } deriving (Show, Generic)
    
instance FromRow where 
    fromRow = Galaxy <$> field <*> field <*> field <*> field

instance ToRow where
    toRow (Galaxy _ name type note) = toRow(name, type, note)

initGalaxyDB :: Connection -> IO ()
initGalaxyDB conn = execute_ conn "CREATE TABLE IF NOT EXISTS galaxies (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, type TEXT NOT NULL, note TEXT NULL)"

insertGalaxy :: Connection -> Galaxy -> IO ()
insertGalaxy conn galaxy = execute conn "INSERT INTO galaxies (name, type, note) VALUES (?, ?, ?)" (toRow galaxy)

getGalaxies :: Connection -> IO [Galaxy]
getGalaxies conn = query_ conn "SELECT * FROM galaxies"

getGalaxyById :: Connection -> Int -> IO (Maybe Galaxy)
getGalaxyById conn id = do 
    result <- query "SELECT * FROM galaxies WHERE id = ?" (Only id)
    return $ case result of
        [galaxy] -> Just galaxy
        _        -> Nothing

updateGalaxy :: Connection -> Int -> Galaxy -> IO ()
updateGalaxy conn id galaxy = 
    execute conn "UPDATE galaxies SET name = ?, type = ?, note = ? WHERE id = ?" (name galaxy, type galaxy, note galaxy, id)

deleteGalaxy :: Connection -> Int -> IO ()
deleteGalaxy conn id =
    execute conn "DELETE FROM galaxies WHERE id = ?" (Only id)
