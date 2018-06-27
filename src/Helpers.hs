module Helpers where

import Data.Time

parseJSONTime :: String -> UTCTime
parseJSONTime s = 
    case mParsed of
        Nothing -> UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
        Just t  -> t
    where mParsed = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-T%-QZ" s

flatten :: [[a]] -> [a]
flatten xs = foldl (\x y -> x ++ y) [] xs