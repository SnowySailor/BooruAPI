{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Database.PostgreSQL.Simple as P
import Data.Either as E
import Data.Yaml
import Datas

getDatabaseCreds :: IO DatabaseCredentials
getDatabaseCreds = do
    creds <- decodeFileEither "./database.yaml" :: IO (E.Either ParseException [DatabaseCredentials])
    return . head $ either (error . show) id creds


getConnectInfo :: IO P.ConnectInfo
getConnectInfo = do
    creds <- getDatabaseCreds
    return P.defaultConnectInfo {
            P.connectHost = "0.0.0.0",
            P.connectUser = db_user creds,
            P.connectPassword = db_password creds,
            P.connectPort = 3306
        }

getSettings :: IO Settings
getSettings = do
    creds <- decodeFileEither "./secrets.yaml" :: IO (E.Either ParseException [Settings])
    return . head $ either (error . show) id creds