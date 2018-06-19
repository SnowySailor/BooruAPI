module Main where

import DerpAPI

main :: IO ()
main = do
    user <- getUserWithFaves 216871
    print user
