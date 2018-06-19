module APIGetter where

import Datas
import Network.HTTP.Conduit
import Network.URI.Encode (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Yaml (decodeFileEither, ParseException)
import Data.Either as E

getAPIKey :: IO APIKey
getAPIKey = do
    creds <- decodeFileEither "./secrets.yaml" :: IO (E.Either ParseException [APIKey])
    return . head $ either (error . show) id creds

extractKey :: APIKey -> String
extractKey = (\(APIKey s) -> s) 

baseUrl :: String
baseUrl = "https://derpibooru.org"

imageAPI :: (Print a) => a -> IO String
imageAPI i = do
    key <- getAPIKey
    return $ baseUrl ++ "/images/" ++ (toString i) ++ ".json?key=" ++ (toString . extractKey $ key)

commentsAPI :: (Print a, Print b) => a -> b -> IO String
commentsAPI i p = do
    key <- getAPIKey
    return $ baseUrl ++ "/images/" ++ (toString i) ++ "/comments.json?page=" ++ (toString p) ++ "&key=" ++ (toString . extractKey $ key)

tagsAPI :: (Print a) => a -> IO String
tagsAPI p = do
    key <- getAPIKey
    return $ baseUrl ++ "/tags.json?page=" ++ (toString p) ++ "&key=" ++ (toString . extractKey $ key)

userAPI :: (Print a) => a -> IO String
userAPI i = do
    key <- getAPIKey
    return $ baseUrl ++ "/profiles/" ++ (toString i) ++ ".json?key=" ++ (toString . extractKey $ key)

searchAPI :: (Print a) => String -> a -> IO String
searchAPI q p = do
    key <- getAPIKey
    return $ baseUrl ++ "/search/index.json?perpage=50&page=" ++ (toString p) ++ "&q=" ++ (encode q) ++ "&key=" ++ (toString . extractKey $ key)

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