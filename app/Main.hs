module Main where

import DerpAPI
import Database.Pool
import Database.Loader

main :: IO ()
main = do
    resource <- defaultResources
    pool     <- getPool resource "derpibooru"
    image    <- getImage 1622923
    print image
    --result   <- withPool pool $ loadImageTags image
    --print result
