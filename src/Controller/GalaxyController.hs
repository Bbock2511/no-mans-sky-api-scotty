{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.GalaxyController
    ( galaxyRoutes
    ) where

import Web.Scotty
import Model.Galaxy
import View.GalaxyView
import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status404)
import Data.Text (Text)
import GHC.Generics (Generic)

data GalaxyInput = GalaxyInput
    { galaxyNameInput :: Text
    , galaxyTypeInput :: Text
    , galaxyNoteInput :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON GalaxyInput
instance ToJSON GalaxyInput

galaxyRoutes :: Connection -> ScottyM ()
galaxyRoutes conn = do
    -- GET /galaxies
    get "/galaxies" $ do
        galaxies <- liftIO $ getGalaxies conn
        json (map galaxyToJSON galaxies)

    -- GET /galaxies/:id
    get "/galaxies/:id" $ do
        gid <- captureParam "id"
        galaxy <- liftIO $ getGalaxyById conn gid
        case galaxy of
            Just g  -> json(galaxyToJSON g)
            Nothing -> status status404 >> text "Galáxia não encontrada"

    -- POST /galaxies
    post "/galaxies" $ do
        -- lê JSON do corpo da requisição
        galaxyInput <- jsonData :: ActionM GalaxyInput
        let gi = GalaxyInsert
                    { giName = galaxyNameInput galaxyInput
                    , giType = galaxyTypeInput galaxyInput
                    , giNote = galaxyNoteInput galaxyInput
                    }
        liftIO $ insertGalaxy conn gi
        json galaxyInput

    -- PUT /galaxies/:id
    put "/galaxies/:id" $ do
        gid <- captureParam "id"
        galaxyInput <- jsonData :: ActionM GalaxyInput
        let gi = GalaxyInsert
                    { giName = galaxyNameInput galaxyInput
                    , giType = galaxyTypeInput galaxyInput
                    , giNote = galaxyNoteInput galaxyInput
                    }
        liftIO $ updateGalaxy conn gid gi
        json galaxyInput
    
    -- DELETE /galaxies/:id
    delete "/galaxies/:id" $ do
        gid <- captureParam "id"
        liftIO $ deleteGalaxy conn gid
        text "Galáxia deletada"
    