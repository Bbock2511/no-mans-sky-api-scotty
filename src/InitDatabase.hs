{-# LANGUAGE OverloadedStrings #-}

module InitDatabase (initDB) where

import Database.SQLite.Simple (Connection, execute_)

initDB :: Connection -> IO ()
initDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS msgs ( \
                  \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \ text TEXT \
                  \ )"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS galaxies ( \
                  \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \ name TEXT NOT NULL, \
                  \ galaxyType TEXT NOT NULL, \
                  \ note TEXT \
                  \ )"

    execute_ conn "CREATE TABLE IF NOT EXISTS solar_systems ( \
                  \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \ name TEXT NOT NULL, \
                  \ race TEXT NOT NULL, \
                  \ economy TEXT NOT NULL, \
                  \ conflict TEXT NOT NULL, \
                  \ note TEXT, \
                  \ galaxy_id INTEGER NOT NULL, \
                  \ FOREIGN KEY(galaxy_id) REFERENCES galaxies(id) \
                  \ )"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS planets ( \
                  \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \ name TEXT NOT NULL, \
                  \ weather TEXT NOT NULL, \
                  \ sentinels TEXT NOT NULL, \
                  \ flora TEXT NOT NULL, \
                  \ fauna TEXT NOT NULL, \
                  \ resources TEXT NOT NULL, \
                  \ notes TEXT, \
                  \ solar_system_id INTEGER NOT NULL, \
                  \ FOREIGN KEY(solar_system_id) REFERENCES solar_systems(id) \
                  \ )"
