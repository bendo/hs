{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Web.Spock
import Web.Spock.Config
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get,delete)
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get,delete)
import Database.Persist.TH
import Network.HTTP.Types.Status

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name Text
    age Int
    deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
    runSpock 3030 (spock spockCfg app)

app :: Api
app = do
    get "people" $ do
        allPeople <- runSQL $ selectList [] [Asc PersonId]
        setStatus status200
        json allPeople
    get ("people" <//> var) $ \personId -> do
        maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do
                setStatus status404
                errorJson 2 "Could not find a person with matching id"
            Just thePerson -> do
                setStatus status200
                json thePerson
    post "people" $ do
        maybePerson <- jsonBody :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do
                setStatus status400
                errorJson 1 "Failed to parse request body as Person"
            Just thePerson -> do
                newId <- runSQL $ insert thePerson
                setStatus status201
                json $ object ["result" .= String "success", "id" .= newId]
    delete ("people" <//> var) $ \personId -> do
        _ <- runSQL $ P.delete (toSqlKey personId :: PersonId)
        json $ object ["result" .= String "success"]
    put ("people" <//> var) $ \personId -> do
        maybePerson <- jsonBody :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do
                setStatus status400
                errorJson 1 "Failed to parse request body as Person"
            Just thePerson -> do
                setStatus status201
                _ <- runSQL $ P.replace personId thePerson
                json $ object ["result" .= String "success"]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
    json $
        object
        [ "result" .= String "failure"
        , "error" .= object ["code" .= code, "message" .= message]
        ]
