{-# LANGUAGE DeriveGeneric #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import GHC.Generics
import Data.Text (Text)

data SolarSystem = SolarSystem
    { systemId     :: Int
    , systemName   :: Text
    , systemRace   :: Text
    , systemEconomy:: Text
    , systemConflict :: Text
    , galaxyIdFk   :: Int   -- chave estrangeira para Galaxy
    } deriving (Show, Generic)

-- Para SELECTs
instance FromRow SolarSystem where
  fromRow = SolarSystem <$> field <*> field <*> field <*> field <*> field <*> field

-- Para INSERTs
data SolarSystemInsert = SolarSystemInsert
    { siName     :: Text
    , siRace     :: Text
    , siEconomy  :: Text
    , siConflict :: Text
    , siGalaxyId :: Int
    } deriving (Show, Generic)

instance ToRow SolarSystemInsert where
  toRow (SolarSystemInsert n r e c gid) = toRow (n, r, e, c, gid)
