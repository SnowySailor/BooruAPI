module DerpAPI 
    (
        getImage,
        getImageFull,
        getImageComments,
        getCommentPage,
        getUserByName,
        getUser,
        getUserFull,
        getUserFavorites,
        getUserFavoritesById,
        getUserFavoritesByName,
        getAllTags
    ) where

import Datas
import Helpers
import APIGetter
import Config
import DataManipulation
import Network.URI.Encode

-- Images

getImage :: ImageId -> IO Image
getImage i = do
    imageData <- getImageJSON i
    return $ decodeNoMaybe imageData

getImageFull :: ImageId -> IO ImageFull
getImageFull i = do
    image <- getImage i
    comments  <- case image of
        Image NullImageData -> return []
        Image d             -> if (image_comment_count d) > 0 then
                                    getImageComments (image_id d) (image_comment_count d)
                                else
                                    return []
        _                   -> return []
    return $ case image of
        Image NullImageData              -> ImageFull NullImageData comments
        Image d                          -> ImageFull d comments
        ImageDuplicate NullDuplicateData -> ImageDuplicateFull NullDuplicateData
        ImageDuplicate d                 -> ImageDuplicateFull d
        ImageDeleted NullDeletedData     -> ImageDeletedFull NullDeletedData
        ImageDeleted d                   -> ImageDeletedFull d
        NullImage                        -> NullImageFull

getImageComments :: ImageId -> Int -> IO [Comment]
getImageComments i count = do
    sett <- getSettings
    let (p, _) = divMod count $ getCommentsPerPage sett
    comments <- mapM (getCommentPage i) [1..(p+1)]
    return . flatten . map (\(CommentPage c) -> c) $ filterNulls comments

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

getUserFull :: (Print a) => a -> IO UserFull
getUserFull i = do
    json <- getUserJSON i
    let user_data = decodeNoMaybe json
    faves <- case user_data of
        NullUserData -> return []
        UserData{}   -> getUserFavorites $ user_name user_data
    return $ UserFull user_data faves

getUserByName :: Username -> IO User
getUserByName s = do
    json <- getUserJSON $ encode s
    return $ decodeNoMaybe json

getUserFavoritesById :: UserId -> IO [ImageId]
getUserFavoritesById i = do
    user <- getUser i
    case user of
        User NullUserData -> return []
        User d            -> getUserFavorites $ user_name d
        _                 -> return []

getUserFavoritesByName :: Username -> IO [ImageId]
getUserFavoritesByName u = getUserFavorites u

getUserFavorites :: Username -> IO [ImageId]
getUserFavorites u = do
    sett <- getSettings
    firstPage  <- getSearchPage q 1
    totalCount <- return $ case firstPage of
        NullSearchPage -> 0
        SearchPage c _ -> c
    let (p, _) = divMod totalCount $ getImagesPerPage sett
    userFaves  <- mapM (getSearchPage q) [2..(p+1)]
    return . map getImageId . flatten . map getSearchImages $ firstPage:(filterNulls userFaves)
    where q = "faved_by:" ++ u

-- Tags

getAllTags :: PageNo -> IO [Tag]
getAllTags maxPage = do
    tagPages <- mapM getTagPage [1..maxPage]
    return . filterNulls . flatten $ map getTagPageTags tagPages

getTagPage :: PageNo -> IO TagPage
getTagPage p = do
    json <- getTagsJSON p
    return $ decodeNoMaybe json

-- Search

getSearchPage :: String -> Int -> IO SearchPage
getSearchPage q i = do
    json <- getSearchJSON q i
    return $ decodeNoMaybe json
