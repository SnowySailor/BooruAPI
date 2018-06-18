module APIGetter where

import Types
import Network.HTTP.Conduit
import Data.ByteString.Lazy

imageAPI :: ImageId -> String
imageAPI i = "https://derpibooru.org/images/" ++ (show i) ++ ".json"

commentsAPI :: ImageId -> PageNo -> String
commentsAPI i p = "https://derpibooru.org/images/" ++ (show i) ++ "/comments.json?page=" ++ (show p)

tagsAPI :: PageNo -> String
tagsAPI p = "https://derpibooru.org/tags.json?page=" ++ (show p)

userAPI :: UserId -> String
userAPI i = "https://derpibooru.org/profiles/" ++ (show i) ++ ".json"

getUserJSON :: UserId -> IO ByteString
getUserJSON i =
    simpleHttp url
    where url = userAPI i

getImageJSON :: ImageId -> IO ByteString
getImageJSON i =
    simpleHttp url
    where url = imageAPI i

getTagsJSON :: PageNo -> IO ByteString
getTagsJSON p =
    simpleHttp url
    where url = tagsAPI p

getCommentsJSON :: ImageId -> PageNo -> IO ByteString
getCommentsJSON i p =
    simpleHttp url
    where url = commentsAPI i p