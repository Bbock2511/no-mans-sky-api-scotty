{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.SolarSystemController
    ( solarSystemRoutes
    ) where

import Web.Scotty
import Model.SolarSystem
import View.SolarSystemView
import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status404)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Estrutura de entrada (payload JSON)
data SolarSystemInput = SolarSystemInput
    { ssNameInput     :: Text
    , ssRaceInput     :: Text
    , ssEconomyInput  :: Text
    , ssConflictInput :: Text
    , ssNotesInput    :: Maybe Text
    , ssGalaxyIdInput :: Int
    } deriving (Show, Generic)

instance FromJSON SolarSystemInput
instance ToJSON SolarSystemInput

solarSystemRoutes :: Connection -> ScottyM ()
solarSystemRoutes conn = do
    -- GET /solar-systems
    get "/solar-systems" $ do
        solarSystems <- liftIO $ getSolarSystems conn
        json (map solarSystemToJSON solarSystems)

    -- GET /solar-systems/:id
    get "/solar-systems/:id" $ do
        sid <- captureParam "id"
        solarSystem <- liftIO $ getSolarSystemByID conn sid
        case solarSystem of
            Just ss -> json(solarSystemToJSON ss)
            Nothing -> status status404 >> text "Sistema solar não encontrado"

    -- POST /solar-systems
    post "/solar-systems" $ do
        input <- jsonData :: ActionM SolarSystemInput
        let si = SolarSystemInsert
                    { siName     = ssNameInput input
                    , siRace     = ssRaceInput input
                    , siEconomy  = ssEconomyInput input
                    , siConflict = ssConflictInput input
                    , siNotes    = ssNotesInput input
                    , siGalaxyId = ssGalaxyIdInput input
                    }
        liftIO $ insertSolarSystem conn si
        json input

    -- PUT /solar-systems/:id
    post "/solar-systems/:id" $ do
        ssid <- captureParam "id"
        input <- jsonData :: ActionM SolarSystemInput
        let si = SolarSystemInsert
                    { siName     = ssNameInput input
                    , siRace     = ssRaceInput input
                    , siEconomy  = ssEconomyInput input
                    , siConflict = ssConflictInput input
                    , siNotes    = ssNotesInput input
                    , siGalaxyId = ssGalaxyIdInput input
                    }
        liftIO $ updateSolarSystem conn ssid si
        json input

    -- DELETE /solar-systems/:id
    delete "/solar-systems/:id" $ do
        ssid <- captureParam "id"
        liftIO $ deleteSolarSystem conn ssid
        text "Sistema solar deletado"