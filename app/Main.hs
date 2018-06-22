module Main where

import DerpAPI
import Data.Pool
import Database.Pool
import Database.Loader

main :: IO ()
main = do
    resource <- defaultResources
    pool     <- getPool resource "derpibooru"
    users    <- mapM getUserFull ([1..5] :: [Int])
    print users
    result   <- withResource pool $ \conn -> mapM (\x -> loadUserFull x conn) users 
    print result
