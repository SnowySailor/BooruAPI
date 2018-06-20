module Main where

import DerpAPI
import Database.Pool
import Database.Loader

main :: IO ()
main = do
    resource <- defaultResources
    pool     <- getPool resource "derpibooru"
    image    <- getImage 1700000
    result   <- withPool pool $ loadImageTags image
    print result
