module APIGetter where

import Datas
import Control.Lens
import Network.Wreq
import Network.URI.Encode (encode)
import Data.ByteString.Lazy (ByteString)

-- Defining request urls
baseUrl :: String
baseUrl = "https://derpibooru.org"

imageAPI :: (Print a) => a -> Settings -> String
imageAPI i s = baseUrl ++ "/images/" ++ (toString i) ++ ".json?key=" ++ (toString $ api_key s)

commentsAPI :: (Print a, Print b) => a -> b -> Settings -> String
commentsAPI i p s = baseUrl ++ "/images/" ++ (toString i) ++ "/comments.json?page=" ++ (toString p) ++ "&key=" ++ (toString $ api_key s)

tagsAPI :: (Print a) => a -> Settings -> String
tagsAPI p s = baseUrl ++ "/tags.json?page=" ++ (toString p) ++ "&key=" ++ (toString $ api_key s)

userAPI :: (Print a) => a -> Settings -> String
userAPI i s = baseUrl ++ "/profiles/" ++ (toString i) ++ ".json?key=" ++ (toString $ api_key s)

searchAPI :: (Print a) => String -> a -> Settings -> String
searchAPI q p s = baseUrl ++ "/search/index.json?perpage=" ++ (toString $ images_per_page s) ++ "&page=" ++ (toString p) ++ "&q=" ++ (encode q) ++ "&key=" ++ (toString $ api_key s)


-- Making Requests
getUserJSON :: (Print a) => a -> Settings -> IO (ByteString, Int)
getUserJSON i s = do
    resp <- get $ userAPI i s
    return (resp ^. responseBody, resp ^. responseStatus . statusCode)

getImageJSON :: (Print a) => a -> Settings -> IO (ByteString, Int)
getImageJSON i s = do
    resp <- get $ imageAPI i s
    return (resp ^. responseBody, resp ^. responseStatus . statusCode)

getTagsJSON :: (Print a) => a -> Settings -> IO (ByteString, Int)
getTagsJSON p s = do
    resp <- get $ tagsAPI p s
    return (resp ^. responseBody, resp ^. responseStatus . statusCode)

getCommentsJSON :: (Print a, Print b) => a -> b -> Settings -> IO (ByteString, Int)
getCommentsJSON i p s = do
    resp <- get $ commentsAPI i p s
    return (resp ^. responseBody, resp ^. responseStatus . statusCode)

getSearchJSON :: (Print a) => String -> a -> Settings -> IO (ByteString, Int)
getSearchJSON s p sett = do
    resp <- get $ searchAPI s p sett
    return (resp ^. responseBody, resp ^. responseStatus . statusCode)
