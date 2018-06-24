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
import DataManipulation
import Network.URI.Encode

-- Images

getImage :: ImageId -> Settings -> IO Image
getImage i s = do
    imageData <- getImageJSON i s
    return $ decodeNoMaybe imageData

getImageFull :: ImageId -> Settings -> IO ImageFull
getImageFull i s = do
    image <- getImage i s
    comments  <- case image of
        Image NullImageData -> return []
        Image d             -> if (image_comment_count d) > 0 then
                                    getImageComments (image_id d) (image_comment_count d) s
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

getImageComments :: ImageId -> Int -> Settings -> IO [Comment]
getImageComments i count s = do
    let (p, _) = divMod count $ comments_per_page s
    comments <- mapM (\x -> getCommentPage i x s) [1..(p+1)]
    return . flatten . map (\(CommentPage c) -> c) $ filterNulls comments

-- Comments

getCommentPage :: ImageId -> PageNo -> Settings -> IO CommentPage
getCommentPage i p s = do
    json <- getCommentsJSON i p s
    return $ decodeNoMaybe json

-- Users

getUser :: UserId -> Settings -> IO User
getUser i s = do
    json <- getUserJSON i s
    return $ decodeNoMaybe json

getUserFull :: (Print a) => a -> Settings -> IO UserFull
getUserFull i s = do
    json <- getUserJSON i s
    let user_data = decodeNoMaybe json
    faves <- case user_data of
        NullUserData -> return []
        UserData{}   -> getUserFavorites (user_name user_data) s
    return $ UserFull user_data faves

getUserByName :: Username -> Settings -> IO User
getUserByName n s = do
    json <- getUserJSON (encode n) s
    return $ decodeNoMaybe json

getUserFavoritesById :: UserId -> Settings -> IO [ImageId]
getUserFavoritesById i s = do
    user <- getUser i s
    case user of
        User NullUserData -> return []
        User d            -> getUserFavorites (user_name d) s
        _                 -> return []

getUserFavoritesByName :: Username -> Settings -> IO [ImageId]
getUserFavoritesByName u s = getUserFavorites u s

getUserFavorites :: Username -> Settings -> IO [ImageId]
getUserFavorites u s = do
    firstPage  <- getSearchPage q 1 s
    totalCount <- return $ case firstPage of
        NullSearchPage -> 0
        SearchPage c _ -> c
    let (p, _) = divMod totalCount $ images_per_page s
    userFaves  <- mapM (\x -> getSearchPage q x s) [2..(p+1)]
    return . map getImageId . flatten . map getSearchImages $ firstPage:(filterNulls userFaves)
    where q = "faved_by:" ++ u

-- Tags

getAllTags :: PageNo -> Settings -> IO [Tag]
getAllTags maxPage s = do
    tagPages <- mapM (\x -> getTagPage x s) [1..maxPage]
    return . filterNulls . flatten $ map getTagPageTags tagPages

getTagPage :: PageNo -> Settings -> IO TagPage
getTagPage p s = do
    json <- getTagsJSON p s
    return $ decodeNoMaybe json

-- Search

getSearchPage :: String -> Int -> Settings -> IO SearchPage
getSearchPage q i s = do
    json <- getSearchJSON q i s
    return $ decodeNoMaybe json
