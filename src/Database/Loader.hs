module Database.Loader where

import Sql
import Datas
import GHC.Int
import Database.Logger
import Database.PostgreSQL.Simple as P
import Processing

-- Images
    -- ImageData
        -- Standard images
        -- Duplicate images
        -- Deleted images
    -- ImageFull
        -- ImageData
        -- Comments
    -- Image tags

loadImage :: Image -> Connection -> String -> OutQueue -> IO (Int64, Int64)
loadImage image conn s out =
    case image of
        Image d -> loadImageData d conn s out
        DuplicateImage d -> do
            dataLoaded <- loadDuplicateData d conn s out
            return (dataLoaded, 0)
        DeletedImage d -> do
            dataLoaded <- loadDeletedData d conn s out
            return (dataLoaded, 0)
        NullImage -> do
            writeOut out "loadImage called on NullImage"
            return (0, 0)

loadImageData :: ImageData -> Connection -> String -> OutQueue -> IO (Int64, Int64)
loadImageData image conn s out =
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
            writeOut out "loadImageData called on NullImageData"
            return (0, 0)

loadDuplicateData :: DuplicateImageData -> Connection -> String -> OutQueue -> IO Int64
loadDuplicateData image conn s out =
    case image of
        DuplicateImageData{} ->
            execute conn (insertImageDuplicate s) (duplicate_image_id image, duplicate_of_id image, duplicate_uploader_id image,
                                                   duplicate_created_at image, duplicate_updated_at image, duplicate_first_seen_at image)
        NullDuplicateImageData -> do
            writeOut out "loadDuplicateData called on NullDuplicateData"
            return 0

loadDeletedData :: DeletedImageData -> Connection -> String -> OutQueue -> IO Int64
loadDeletedData image conn s out =
    case image of
        DeletedImageData{} ->
            execute conn (insertImageDeleted s) (deleted_image_id image, deleted_uploader_id image, deleted_reason image, deleted_created_at image,
                                                 deleted_updated_at image, deleted_first_seen_at image)
        NullDeletedImageData -> do
            writeOut out "loadDeletedData called on NullDeletedData"
            return 0

loadImageTags :: Image -> Connection -> String -> OutQueue -> IO Int64
loadImageTags image conn s out =
    case image of
        Image d -> loadTags (image_tags d) (image_id d) conn s
        DuplicateImage _ -> do
            writeOut out "loadImageTags called on ImageDuplicate"
            return 0
        DeletedImage _ -> do
            writeOut out "loadImageTags called on ImageDeleted"
            return 0
        NullImage -> do
            writeOut out "loadImageTags called on ImageNull"
            return 0

-- Tags
    -- Image tags
    -- Tag definitions
    -- Implied tags

loadTags :: [TagId] -> ImageId -> Connection -> String -> IO Int64
loadTags tags image conn s =
    executeMany conn (insertImageTag s) tagsToLoad
    where tagsToLoad = map (\x -> (image, x)) tags

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

loadUserFavorites :: UserId -> [ImageId] -> Connection -> String -> OutQueue -> IO Int64
loadUserFavorites user f conn s out =
    executeMany conn (insertUserFavorite s) faves
    where faves = map (\x -> (user, x)) f

loadUserAwards :: User -> Connection -> String -> OutQueue -> IO Int64
loadUserAwards user conn s out =
    case user of
        User{} ->
            executeMany conn (insertUserAward s) awards
            where awards = map (\x -> (award_id x, user_id user, award_title x, award_label x, award_date x)) $ user_awards user
        AnonymousUser -> do
            writeOut out "loadUserAwards called on AnonymousUserFull"
            return 0
        NullUser -> do
            writeOut out "loadUserAwards called on NullUserFull"
            return 0

loadUserLinks :: User -> Connection -> String -> OutQueue -> IO Int64
loadUserLinks user conn s out =
    case user of
        User{} ->
            executeMany conn (insertUserLink s) links
            where links = map (\x -> (link_user_id x, link_tag_id x, link_created_at x, link_state x)) $ user_links user
        AnonymousUser -> do
            writeOut out "loadUserLinks called on AnonymousUser"
            return 0
        NullUser -> do
            writeOut out "loadUserLinks called on NullUser"
            return 0

loadUser :: User -> Connection -> String -> OutQueue -> IO (Int64, Int64, Int64)
loadUser user conn s out =
    case user of
        User{} ->
            withTransaction conn $ do
                dataLoaded   <- execute conn (insertUser s) (user_id user, user_name user, user_description user, user_role user,
                                                             user_created_at user, user_comment_count user, user_upload_count user,
                                                             user_post_count user, user_topic_count user)
                awardsLoaded <- loadUserAwards user conn s out
                linksLoaded  <- loadUserLinks user conn s out
                return (dataLoaded, awardsLoaded, linksLoaded)
        AnonymousUser -> do
            writeOut out "loadUser called on AnonymousUser"
            return (0,0,0)
        NullUser -> do
            writeOut out "loadUser called on NullUser"
            return (0,0,0)
