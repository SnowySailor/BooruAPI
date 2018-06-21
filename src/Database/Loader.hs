module Database.Loader where

import Sql
import Datas
import GHC.Int
import Database.Logger
import Database.PostgreSQL.Simple as P

-- Images
    -- ImageData
        -- Standard images
        -- Duplicate images
        -- Deleted images
    -- ImageFull
        -- ImageData
        -- Comments
    -- Image tags

loadImageData :: ImageData -> Connection -> IO Int64
loadImageData = undefined

loadDuplicateData :: DuplicateData -> Connection -> IO Int64
loadDuplicateData = undefined

loadDeletedData :: DeletedData -> Connection -> IO Int64
loadDeletedData = undefined

loadComments :: [Comment] -> Connection -> IO Int64
loadComments = undefined

loadImageTags :: Image -> Connection -> IO Int64
loadImageTags image conn = do
    case image of
        Image d -> do
            executeMany conn insertImageTag tags
            where tags = map (\x -> ((image_id d), x)) $ image_tags d
        ImageDuplicate _ -> do
            logError "loadImageTags called on ImageDuplicate"
            return 0
        ImageDeleted _ -> do
            logError "loadImageTags called on ImageDeleted"
            return 0
        NullImage -> do
            logError "loadImageTags called on NullImage"
            return 0

loadImage :: Image -> Connection -> IO Int64
loadImage = undefined

-- Returns a tuple with the number of image datas loaded and the number of comments loaded respectively
loadImageFull :: ImageFull -> Connection -> IO (Int64, Int64)
loadImageFull image conn = do
    case image of
        ImageFull d c -> do
            withTransaction conn $ do
                dataLoaded     <- loadImageData d conn
                commentsLoaded <- loadComments c conn
                return (dataLoaded, commentsLoaded)
        ImageDuplicateFull d -> do
            dataLoaded <- loadDuplicateData d conn
            return (dataLoaded, 0)
        ImageDeletedFull d -> do
            dataLoaded <- loadDeletedData d conn
            return (dataLoaded, 0)
        NullImageFull -> do
            logError "loadImageFull called on NullImageFull"
            return (0, 0)

-- Comments
    -- Comments

-- Users
    -- Profiles
    -- Awards
    -- Links
    -- Favorites

loadUser :: User -> IO [Int64]
loadUser = undefined

loadUserFavorites :: UserFull -> Connection -> IO Int64
loadUserFavorites user conn = do
    case user of
        UserFull u f -> do
            executeMany conn insertUserFavorite faves
            where faves = map (\x -> ((user_id u), x)) f
        AnonymousUserFull -> do
            logError "loadUserFavorites called on AnonymousUserFull"
            return 0
        NullUserFull -> do
            logError "loadUserFavorites called on NullUserFull"
            return 0

-- Tags
    -- Tag definitions
    -- Implied tags

