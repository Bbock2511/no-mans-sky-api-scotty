{-# LANGUAGE OverloadedStrings #-}

module Model.SolarSystemSpec (solarSystemTests) where

import Test.HUnit
import Database.SQLite.Simple

-- Importa o módulo sob teste e sua dependência (Galaxy)
import Model.Galaxy
import Model.SolarSystem

-- Função auxiliar que prepara o DB com as tabelas necessárias para os testes de SolarSystem
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = withConnection ":memory:" $ \conn -> do
    -- Setup: Criar as tabelas na ordem de dependência
    execute_ conn "CREATE TABLE galaxies (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, galaxyType TEXT, note TEXT)"
    execute_ conn "CREATE TABLE solar_systems (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, race TEXT, economy TEXT, conflict TEXT, note TEXT, galaxy_id INTEGER, FOREIGN KEY(galaxy_id) REFERENCES galaxies(id))"
    
    -- Executa a ação de teste
    action conn

-- Suíte de testes completa para o CRUD de SolarSystem
solarSystemCrudTests :: Test
solarSystemCrudTests = TestList
    [ "Test insert and getById SolarSystem" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange: Primeiro, crie a galáxia pai para a chave estrangeira.
        insertGalaxy conn (GalaxyInsert "Euclid" "Normal" Nothing) -- Receberá o ID 1
        let newSystem = SolarSystemInsert "Sol" "Human" "Trading" "Low" (Just "Sistema inicial") 1

        -- Act: Insira o novo sistema solar.
        insertSolarSystem conn newSystem
        
        -- Assert: Busque pelo ID e verifique os campos.
        retrieved <- getSolarSystemByID conn 1
        case retrieved of
            Nothing -> assertFailure "Não foi possível buscar o sistema solar inserido."
            Just s  -> do
                assertEqual "Nome do sistema incorreto" "Sol" (systemName s)
                assertEqual "Raça do sistema incorreta" "Human" (systemRace s)
                assertEqual "ID da galáxia (FK) incorreto" 1 (galaxyIdFk s)

    , "Test update SolarSystem" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange: Insira uma galáxia e um sistema solar inicial.
        insertGalaxy conn (GalaxyInsert "Euclid" "Normal" Nothing)
        insertSolarSystem conn (SolarSystemInsert "Sol" "Human" "Trading" "Low" Nothing 1)
        
        let updatedInfo = SolarSystemInsert "Alpha Centauri" "Gek" "Mining" "High" (Just "Sistema vizinho") 1

        -- Act: Atualize o sistema solar de ID 1.
        updateSolarSystem conn 1 updatedInfo

        -- Assert: Busque novamente e verifique se os dados foram alterados.
        retrieved <- getSolarSystemByID conn 1
        case retrieved of
            Nothing -> assertFailure "Sistema solar não foi encontrado após o update."
            Just s -> do
                assertEqual "Nome não foi atualizado corretamente" "Alpha Centauri" (systemName s)
                assertEqual "Economia não foi atualizada corretamente" "Mining" (systemEconomy s)

    , "Test delete SolarSystem" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange: Insira os dados necessários.
        insertGalaxy conn (GalaxyInsert "Euclid" "Normal" Nothing)
        insertSolarSystem conn (SolarSystemInsert "Sol" "Human" "Trading" "Low" Nothing 1)

        -- Act: Delete o sistema solar de ID 1.
        deleteSolarSystem conn 1

        -- Assert: Verifique que a busca por esse ID agora retorna Nothing.
        retrieved <- getSolarSystemByID conn 1
        assertEqual "O sistema solar não foi deletado corretamente" Nothing retrieved
    ]

-- Exporta todos os testes do módulo para serem usados no Spec.hs
solarSystemTests :: Test
solarSystemTests = TestLabel "SolarSystem CRUD Tests" solarSystemCrudTests