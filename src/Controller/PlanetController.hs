{-# LANGUAGE OverloadedStrings #-}

module Controller.PlanetController
    ( planetRoutes
    ) where

import Web.Scotty
import Model.Planet
import View.PlanetView
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status404)
import Data.Text()

planetRoutes :: Connection -> ScottyM ()
planetRoutes conn = do
    -- GET /planets
    get "/planets" $ do
        planets <- liftIO $ getPlanets conn
        json (map planetToJSON planets)

    -- GET /planets/:id
    get "/planets/:id" $ do
        pid <- captureParam "id"
        planet <- liftIO $ getPlanetById conn pid
        case planet of
            Just p  -> json (planetToJSON p)
            Nothing -> status status404 >> text "Planeta não encontrado" 

    -- POST /planets
    post "/planets" $ do
        input <- jsonData :: ActionM PlanetInsert
        liftIO $ insertPlanet conn input
        json input
    
    -- PUT /planets/:id
    put "/planets/:id" $ do
        pid <- captureParam "id"
        input <- jsonData :: ActionM PlanetInsert
        liftIO $ updatePlanet conn pid input
        json input

    -- DELETE /planets/:id
    delete "/planets/:id" $ do
        pid <- captureParam "id"
        liftIO $ deletePlanet conn pid
        text "Planeta deletado"

