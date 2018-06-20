module Main where

import DerpAPI

main :: IO ()
main = do
    user <- getUserFull 260155
    print user
