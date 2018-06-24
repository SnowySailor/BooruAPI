module APIGetter where

import Datas
import Network.HTTP.Conduit
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
getUserJSON :: (Print a) => a -> Settings -> IO ByteString
getUserJSON i s = simpleHttp $ userAPI i s

getImageJSON :: (Print a) => a -> Settings -> IO ByteString
getImageJSON i s = simpleHttp $ imageAPI i s

getTagsJSON :: (Print a) => a -> Settings -> IO ByteString
getTagsJSON p s = simpleHttp $ tagsAPI p s

getCommentsJSON :: (Print a, Print b) => a -> b -> Settings -> IO ByteString
getCommentsJSON i p s = simpleHttp $ commentsAPI i p s

getSearchJSON :: (Print a) => String -> a -> Settings -> IO ByteString
getSearchJSON s p sett = simpleHttp $ searchAPI s p sett