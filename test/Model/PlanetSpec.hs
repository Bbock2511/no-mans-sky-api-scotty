{-# LANGUAGE OverloadedStrings #-}

module Model.PlanetSpec (planetTests) where

import Test.HUnit
import Database.SQLite.Simple
import Model.Galaxy
import Model.SolarSystem
import Model.Planet

-- Função auxiliar ajustada para criar o schema completo
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = withConnection ":memory:" $ \conn -> do
    -- 1. Setup: Criar o schema na ordem correta de dependência
    execute_ conn "CREATE TABLE galaxies (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, galaxyType TEXT, note TEXT)"
    execute_ conn "CREATE TABLE solar_systems (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, race TEXT, economy TEXT, conflict TEXT, note TEXT, galaxy_id INTEGER, FOREIGN KEY(galaxy_id) REFERENCES galaxies(id))"
    execute_ conn "CREATE TABLE planets (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, weather TEXT, sentinels TEXT, flora TEXT, fauna TEXT, resources TEXT, notes TEXT, solar_system_id INTEGER, FOREIGN KEY(solar_system_id) REFERENCES solar_systems(id))"
    
    -- 2. Run: Executar a ação de teste
    action conn

-- Suíte de testes para o CRUD de Planet
planetCrudTests :: Test
planetCrudTests = TestList
    [ "Test insert and getById Planet" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange: Precisamos criar uma Galáxia e um Sistema Solar primeiro
        insertGalaxy conn (GalaxyInsert "Euclid" "Normal" Nothing) -- Terá ID 1
        let solarSystem = SolarSystemInsert "Sol" "Human" "Trading" "Low" Nothing 1
        insertSolarSystem conn solarSystem -- Terá ID 1

        let newPlanet = PlanetInsert "Terra" "Temperado" "Baixo" "Rica" "Abundante" "Cobre" Nothing 1

        -- Act: Inserir o planeta
        insertPlanet conn newPlanet

        -- Assert: Buscar o planeta e verificar seus dados
        retrieved <- getPlanetById conn 1
        case retrieved of
            Nothing -> assertFailure "Falha ao buscar planeta inserido."
            Just p -> do
                assertEqual "Nome do planeta incorreto" "Terra" (planetName p)
                assertEqual "Clima do planeta incorreto" "Temperado" (planetWeather p)
                assertEqual "Chave estrangeira (solar_system_id) incorreta" 1 (solarSystemIdFK p)

    , "Test update Planet" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange: Setup completo
        insertGalaxy conn (GalaxyInsert "Euclid" "Normal" Nothing)
        insertSolarSystem conn (SolarSystemInsert "Sol" "Human" "Trading" "Low" Nothing 1)
        insertPlanet conn (PlanetInsert "Terra" "Temperado" "Baixo" "Rica" "Abundante" "Cobre" Nothing 1)
        
        let updatedInfo = PlanetInsert "Marte" "Arido" "Alto" "Escassa" "Inexistente" "Ferro" (Just "Colonizado") 1

        -- Act: Atualizar o planeta de ID 1
        updatePlanet conn 1 updatedInfo

        -- Assert
        retrieved <- getPlanetById conn 1
        case retrieved of
            Nothing -> assertFailure "Planeta não encontrado após update."
            Just p -> do
                assertEqual "Nome não foi atualizado" "Marte" (planetName p)
                assertEqual "Sentinelas não foram atualizadas" "Alto" (planetSentinels p)

    , "Test delete Planet" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange
        insertGalaxy conn (GalaxyInsert "Euclid" "Normal" Nothing)
        insertSolarSystem conn (SolarSystemInsert "Sol" "Human" "Trading" "Low" Nothing 1)
        insertPlanet conn (PlanetInsert "Terra" "Temperado" "Baixo" "Rica" "Abundante" "Cobre" Nothing 1)

        -- Act
        deletePlanet conn 1

        -- Assert
        retrieved <- getPlanetById conn 1
        assertEqual "Planeta não foi deletado corretamente" Nothing retrieved
    ]

-- Exporta todos os testes do módulo
planetTests :: Test
planetTests = TestLabel "Planet CRUD Tests" planetCrudTests