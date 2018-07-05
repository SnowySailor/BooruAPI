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

zipLists :: [a] -> [a] -> [a]
zipLists (xs)   ([])   = xs
zipLists ([])   (ys)   = ys
zipLists (x:xs) (y:ys) = x:y:(zipLists xs ys)
