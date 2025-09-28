{-# LANGUAGE OverloadedStrings #-}

module Model.MsgSpec (msgTests) where

import Test.HUnit
import Database.SQLite.Simple
import Data.Text (Text)
import Model.Msg

-- Função auxiliar para rodar testes com um DB em memória para msgs
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = withConnection ":memory:" $ \conn -> do
    -- Setup: Criar a tabela 'msgs'
    execute_ conn "CREATE TABLE msgs (id INTEGER PRIMARY KEY AUTOINCREMENT, text TEXT NOT NULL)"
    
    -- Executa a ação de teste
    action conn

-- Suíte de testes para o CRUD de Msg
msgCrudTests :: Test
msgCrudTests = TestList
    [ "Test insert and getById Msg" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange
        let initialText = "Olá, mundo dos testes!" :: Text

        -- Act
        insertMsg conn initialText

        -- Assert
        retrieved <- getMsgById conn 1
        case retrieved of
            Nothing -> assertFailure "Não foi possível buscar a mensagem inserida."
            Just msg -> assertEqual "O texto da mensagem está incorreto" initialText (msgText msg)

    , "Test update Msg" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange
        let originalText = "Texto original" :: Text
        let updatedText = "Texto atualizado" :: Text
        insertMsg conn originalText

        -- Act: Atualiza a mensagem de ID 1
        updateMsg conn 1 updatedText

        -- Assert
        retrieved <- getMsgById conn 1
        case retrieved of
            Nothing -> assertFailure "Mensagem não encontrada após o update."
            Just msg -> assertEqual "O texto não foi atualizado corretamente" updatedText (msgText msg)

    , "Test delete Msg" ~: TestCase $ withTestDb $ \conn -> do
        -- Arrange
        insertMsg conn ("Texto para deletar" :: Text)

        -- Act
        deleteMsg conn 1

        -- Assert
        retrieved <- getMsgById conn 1
        assertEqual "A mensagem não foi deletada corretamente" Nothing retrieved
    ]

-- Exporta todos os testes do módulo
msgTests :: Test
msgTests = TestLabel "Msg CRUD Tests" msgCrudTests