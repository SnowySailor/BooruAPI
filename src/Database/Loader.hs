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

loadImageData :: ImageData -> Connection -> String -> IO (Int64, Int64)
loadImageData image conn s =
    case image of
        ImageData{} ->
            withTransaction conn $ do
                tagsLoaded <- loadTags (image_tags image) (image_id image) conn s
                dataLoaded <- execute conn (insertImage s) (image_id image, image_uploader_id image, image_description image, image_upvotes image,
                                                            image_downvotes image, image_faves image, image_score image, image_comment_count image,
                                                            image_created_at image, image_updated_at image, image_first_seen_at image,
                                                            image_width image, image_height image, image_aspect_ratio image)
                return (tagsLoaded, dataLoaded)
        NullImageData -> do
            logError "loadImageData called on NullImageData"
            return (0, 0)

loadDuplicateData :: DuplicateData -> Connection -> String -> IO Int64
loadDuplicateData image conn s =
    case image of
        DuplicateData{} ->
            execute conn (insertImageDuplicate s) (duplicate_image_id image, duplicate_of_id image, duplicate_uploader_id image,
                                                   duplicate_created_at image, duplicate_updated_at image, duplicate_first_seen_at image)
        NullDuplicateData -> do
            logError "loadDuplicateData called on NullDuplicateData"
            return 0

loadDeletedData :: DeletedData -> Connection -> String -> IO Int64
loadDeletedData image conn s =
    case image of
        DeletedData{} ->
            execute conn (insertImageDeleted s) (deleted_image_id image, deleted_uploader_id image, deleted_reason image, deleted_created_at image,
                                                 deleted_updated_at image, deleted_first_seen_at image)
        NullDeletedData -> do
            logError "loadDeletedData called on NullDeletedData"
            return 0

loadImageTags :: Image -> Connection -> String -> IO Int64
loadImageTags image conn s =
    case image of
        Image d -> loadTags (image_tags d) (image_id d) conn s
        ImageDuplicate _ -> do
            logError "loadImageTags called on ImageDuplicate"
            return 0
        ImageDeleted _ -> do
            logError "loadImageTags called on ImageDeleted"
            return 0
        NullImage -> do
            logError "loadImageTags called on ImageNull"
            return 0

loadImage :: Image -> Connection -> String -> IO (Int64, Int64)
loadImage image conn s =
    case image of
        Image d -> loadImageData d conn s
        ImageDuplicate d -> do
            dataLoaded <- loadDuplicateData d conn s
            return (dataLoaded, 0)
        ImageDeleted d -> do
            dataLoaded <- loadDeletedData d conn s
            return (dataLoaded, 0)
        NullImage -> do
            logError "loadImage called on NullImage"
            return (0, 0)

-- Returns a tuple with the number of image datas loaded and the number of comments loaded respectively
loadImageFull :: ImageFull -> Connection -> String -> IO (Int64, Int64, Int64)
loadImageFull image conn s =
    case image of
        ImageFull d c -> do
            withTransaction conn $ do
                (tagsLoaded, dataLoaded) <- loadImageData d conn s
                commentsLoaded           <- loadComments c conn s
                return (dataLoaded, tagsLoaded, commentsLoaded)
        ImageDuplicateFull d -> do
            dataLoaded <- loadDuplicateData d conn s
            return (dataLoaded, 0, 0)
        ImageDeletedFull d -> do
            dataLoaded <- loadDeletedData d conn s
            return (dataLoaded, 0, 0)
        NullImageFull -> do
            logError "loadImageFull called on NullImageFull"
            return (0, 0, 0)

-- Comments
    -- Comments

loadComments :: [Comment] -> Connection -> String -> IO Int64
loadComments c conn s =
    executeMany conn (insertComment s) comments
    where comments = map (\x -> (comment_id x, comment_image_id x, comment_author x,
                                 comment_body x, comment_posted_at x, comment_deleted x)) c

-- Users
    -- Profiles
    -- Awards
    -- Links
    -- Favorites

loadUserFavorites :: UserFull -> Connection -> String -> IO Int64
loadUserFavorites user conn s =
    case user of
        UserFull u f ->
            executeMany conn (insertUserFavorite s) faves
            where faves = map (\x -> ((user_id u), x)) f
        AnonymousUserFull -> do
            logError "loadUserFavorites called on AnonymousUserFull"
            return 0
        NullUserFull -> do
            logError "loadUserFavorites called on NullUserFull"
            return 0

loadUserAwards :: User -> Connection -> String -> IO Int64
loadUserAwards user conn s =
    case user of
        User NullUserData -> do
            logError "loadUserAwards called on NullUserData"
            return 0
        User d ->
            executeMany conn (insertUserAward s) awards
            where awards = map (\x -> (award_id x, user_id d, award_title x, award_label x, award_date x)) $ user_awards d
        AnonymousUser -> do
            logError "loadUserAwards called on AnonymousUserFull"
            return 0
        NullUser -> do
            logError "loadUserAwards called on NullUserFull"
            return 0

loadUserLinks :: User -> Connection -> String -> IO Int64
loadUserLinks user conn s =
    case user of
        User NullUserData -> do
            logError "loadUserLinks called on NullUserData"
            return 0
        User d ->
            executeMany conn (insertUserLink s) links
            where links = map (\x -> (link_user_id x, link_tag_id x, link_created_at x, link_state x)) $ user_links d
        AnonymousUser -> do
            logError "loadUserLinks called on AnonymousUser"
            return 0
        NullUser -> do
            logError "loadUserLinks called on NullUser"
            return 0

loadUser :: User -> Connection -> String -> IO (Int64, Int64, Int64)
loadUser user conn s =
    case user of
        User NullUserData -> do
            logError "loadUser called on NullUserData"
            return (0, 0, 0)
        User d ->
            withTransaction conn $ do
                dataLoaded <-
                    execute conn (insertUser s) (user_id d, user_name d, user_description d, user_role d, user_created_at d,
                                                 user_comment_count d, user_upload_count d, user_post_count d, user_topic_count d)
                awardsLoaded <- loadUserAwards user conn s
                linksLoaded <- loadUserLinks user conn s
                return (dataLoaded, awardsLoaded, linksLoaded)
        AnonymousUser -> do
            logError "loadUser called on AnonymousUser"
            return (0,0,0)
        NullUser -> do
            logError "loadUser called on NullUser"
            return (0,0,0)

loadUserFull :: UserFull -> Connection -> String -> IO (Int64, Int64, Int64, Int64)
loadUserFull user conn s =
    case user of
        UserFull d _ ->
            withTransaction conn $ do
                (dataLoaded, awardsLoaded, linksLoaded) <- loadUser (User d) conn s
                favesLoaded <- loadUserFavorites user conn s
                return (dataLoaded, awardsLoaded, linksLoaded, favesLoaded)
        AnonymousUserFull -> do
            logError "loadUserFull called on AnonymousUserFull"
            return (0, 0, 0, 0)
        NullUserFull -> do
            logError "loadUserFull called on NullUserFull"
            return (0, 0, 0, 0)

-- Tags
    -- Image tags
    -- Tag definitions
    -- Implied tags

loadTags :: [TagId] -> ImageId -> Connection -> String -> IO Int64
loadTags tags img_id conn s =
    executeMany conn (insertImageTag s) tagsToLoad
    where tagsToLoad = map (\x -> (img_id, x)) tags
