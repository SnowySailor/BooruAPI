module DerpAPI 
    (
        getImage,
        getImageComments,
        getImageWithComments,
        getUserByName,
        getUser,
        getUserFavorites,
        getUserFavoritesById,
        getUserFavoritesByName
    ) where

import Datas
import Helpers
import APIGetter
import DataManipulation
import Network.URI.Encode

-- Images

getImage :: ImageId -> IO ImageData
getImage i = do
    imageData <- getImageJSON i
    return $ decodeNoMaybe imageData

getImageWithComments :: ImageId -> IO ImageWithComments
getImageWithComments i = do
    imageData <- getImage i
    comments  <- case imageData of
        (ImageData d) -> getImageComments (image_id d) (image_comment_count d)
        _             -> return []
    return $ ImageWithComments imageData comments
getComments d = do
    return $ ImageWithComments d []

getImageComments :: ImageId -> Int -> IO [Comment]
getImageComments i count = do
    comments <- mapM (getCommentPage i) [1..(p+1)]
    return $ flatten $ map (\(CommentPage c) -> c) comments
    where (p, _) = divMod count 50

-- Comments

getCommentPage :: ImageId -> PageNo -> IO CommentPage
getCommentPage i p = do
    json <- getCommentsJSON i p
    return $ decodeNoMaybe json

-- Users

getUser :: UserId -> IO User
getUser i = do
    json <- getUserJSON i
    return $ decodeNoMaybe json

getUserByName :: Username -> IO User
getUserByName s = do
    json <- getUserJSON $ encode s
    return $ decodeNoMaybe json

getUserFavoritesById :: UserId -> IO [ImageId]
getUserFavoritesById i = do
    user <- getUser i
    case user of
        NullUser -> return []
        User{}   -> getUserFavorites $ user_name user

getUserFavoritesByName :: Username -> IO [ImageId]
getUserFavoritesByName u = getUserFavorites u

getUserFavorites :: Username -> IO [ImageId]
getUserFavorites u = do
    firstPage  <- getSearchPage q 1
    totalCount <- return $ case firstPage of
        NullSearchPage -> 0
        SearchPage c _ -> c
    let (p, _) = divMod totalCount 20
    userFaves  <- mapM (getSearchPage q) [2..(p+1)]
    return $ map getImageId . flatten . map getSearchImages $ firstPage:userFaves
    where q = "faved_by:" ++ u

-- Tags

getAllTags :: IO [Tag]
getAllTags = undefined

-- Search

getSearchPage :: String -> Int -> IO SearchPage
getSearchPage q i = do
    json <- getSearchJSON q i
    return $ decodeNoMaybe json
