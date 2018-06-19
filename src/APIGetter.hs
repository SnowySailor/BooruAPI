module APIGetter where

import Datas
import Config
import Network.HTTP.Conduit
import Network.URI.Encode (encode)
import Data.ByteString.Lazy (ByteString)

getKey :: Settings -> String
getKey = (\(Settings s _ _) -> s)

getImagesPerPage :: Settings -> Int
getImagesPerPage = (\(Settings _ i _) -> i)

getCommentsPerPage :: Settings -> Int
getCommentsPerPage = (\(Settings _ _ i) -> i)

baseUrl :: String
baseUrl = "https://derpibooru.org"

imageAPI :: (Print a) => a -> IO String
imageAPI i = do
    sett <- getSettings
    return $ baseUrl ++ "/images/" ++ (toString i) ++ ".json?key=" ++ (toString . getKey $ sett)

commentsAPI :: (Print a, Print b) => a -> b -> IO String
commentsAPI i p = do
    sett <- getSettings
    return $ baseUrl ++ "/images/" ++ (toString i) ++ "/comments.json?page=" ++ (toString p) ++ "&key=" ++ (toString . getKey $ sett)

tagsAPI :: (Print a) => a -> IO String
tagsAPI p = do
    sett <- getSettings
    return $ baseUrl ++ "/tags.json?page=" ++ (toString p) ++ "&key=" ++ (toString . getKey $ sett)

userAPI :: (Print a) => a -> IO String
userAPI i = do
    sett <- getSettings
    return $ baseUrl ++ "/profiles/" ++ (toString i) ++ ".json?key=" ++ (toString . getKey $ sett)

searchAPI :: (Print a) => String -> a -> IO String
searchAPI q p = do
    sett <- getSettings
    return $ baseUrl ++ "/search/index.json?perpage=" ++ (toString . getImagesPerPage $ sett) ++ "&page=" ++ (toString p) ++ "&q=" ++ (encode q) ++ "&key=" ++ (toString . getKey $ sett)

getUserJSON :: (Print a) => a -> IO ByteString
getUserJSON i = do
    url <- userAPI i
    simpleHttp url

getImageJSON :: (Print a) => a -> IO ByteString
getImageJSON i = do
    url <- imageAPI i
    simpleHttp url

getTagsJSON :: (Print a) => a -> IO ByteString
getTagsJSON p = do
    url <- tagsAPI p
    simpleHttp url

getCommentsJSON :: (Print a, Print b) => a -> b -> IO ByteString
getCommentsJSON i p = do
    url <- commentsAPI i p
    simpleHttp url

getSearchJSON :: (Print a) => String -> a -> IO ByteString
getSearchJSON s p = do
    url <- searchAPI s p
    simpleHttp url