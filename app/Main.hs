module Main where

import DerpAPI
import Datas
import Data.Pool
import Database.Pool
import Database.Loader
import Config

main :: IO ()
main = do
    resource <- defaultResources
    settings <- getSettings
    creds    <- getDatabaseCreds
    let schema = db_schema creds
    pool     <- getPool resource "derpibooru"
    result   <- getUserFull ("AdamAzure" :: String) settings
    print result
    insert_result   <- withResource pool $ \conn -> loadUserFull result conn schema
    print insert_result
