{-# LANGUAGE OverloadedStrings #-}

module Model.GalaxySpec (galaxyTests) where

import Test.HUnit
import Database.SQLite.Simple
import Model.Galaxy

-- Função auxiliar para rodar um teste com um DB em memória limpo
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = withConnection ":memory:" $ \conn -> do
    -- 1. Setup: Criar o schema
    execute_ conn "CREATE TABLE galaxies (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, galaxyType TEXT, note TEXT)"
    -- 2. Run: Executar a ação de teste
    action conn

-- Nossa suíte de testes para o CRUD de Galaxy
galaxyCrudTests :: Test
galaxyCrudTests = TestList
    [ "Test insert and getById" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange: Dados de teste
        let newGalaxy = GalaxyInsert "Andromeda" "Spiral" (Just "Vizinha")
        
        -- Act: Inserir no banco
        insertGalaxy conn newGalaxy
        
        -- Assert: Buscar e verificar
        retrieved <- getGalaxyById conn 1
        case retrieved of
            Nothing -> assertFailure "Falha ao buscar galáxia inserida."
            Just g  -> do
                assertEqual "Nome da galáxia incorreto" "Andromeda" (galaxyName g)
                assertEqual "Tipo da galáxia incorreto" "Spiral" (galaxyType g)

    , "Test update" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange
        let initialGalaxy = GalaxyInsert "Andromeda" "Spiral" Nothing
        let updatedInfo = GalaxyInsert "Andromeda II" "Dwarf" (Just "Atualizada")
        insertGalaxy conn initialGalaxy

        -- Act
        updateGalaxy conn 1 updatedInfo

        -- Assert
        retrieved <- getGalaxyById conn 1
        case retrieved of
            Nothing -> assertFailure "Galáxia não encontrada após update."
            Just g -> assertEqual "Nome não foi atualizado corretamente" "Andromeda II" (galaxyName g)

    , "Test delete" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange
        insertGalaxy conn (GalaxyInsert "Andromeda" "Spiral" Nothing)

        -- Act
        deleteGalaxy conn 1

        -- Assert
        retrieved <- getGalaxyById conn 1
        assertEqual "Galáxia não foi deletada" Nothing retrieved
    ]

-- Exporta todos os testes do módulo
galaxyTests :: Test
galaxyTests = TestLabel "Galaxy CRUD TestSs" galaxyCrudTests