{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Datas where

import Data.Aeson
import Data.Time.Clock
import Data.Attoparsec.ByteString

-- Types

type PageNo    = Int
type ImageId   = Int
type CommentId = Int
type TagId     = Int
type AwardId   = Int
type UserId    = Int
type Username  = String

-- Datas

data Settings          = Settings String Int deriving (Show)
data CommentPage       = CommentPage [Comment] | NullCommentPage deriving (Show)
data SearchPage        = SearchPage Int [ImageData] | NullSearchPage deriving (Show)
data ImageWithComments = ImageWithComments ImageData [Comment] deriving (Show)
data ImageData         = ImageData Data | ImageDuplicateData DuplicateData | ImageNullData deriving (Show)

data Data = Data {
    image_id            :: ImageId   ,
    image_upvotes       :: Int       ,
    image_downvotes     :: Int       ,
    image_faves         :: Int       ,
    image_score         :: Int       ,
    image_height        :: Int       ,
    image_width         :: Int       ,
    image_aspect_ratio  :: Double    ,
    image_description   :: String    ,
    image_created_at    :: UTCTime   ,
    image_updated_at    :: UTCTime   ,
    image_first_seen_at :: UTCTime   ,
    image_tags          :: [TagId]   ,
    image_uploader      :: Maybe Int ,
    image_comment_count :: Int
} deriving (Show)

data DuplicateData = DuplicateData {
    duplicate_image_id      :: Int    ,
    duplicate_created_at    :: UTCTime,
    duplicate_updated_at    :: UTCTime,
    duplicate_first_seen_at :: UTCTime,
    duplicate_of_id         :: Int    ,
    duplicate_uploader_id   :: Maybe Int
} deriving (Show)

data Tag = Tag {
    tag_id                :: TagId  ,
    tag_name              :: String ,
    tag_slug              :: String ,
    tag_short_desctiption :: String ,
    tag_description       :: String ,
    tag_implied_tags      :: [TagId],
    tag_aliased_to        :: Maybe TagId
} | NullTag deriving (Show)

data Comment = Comment {
    comment_id        :: CommentId,
    comment_posted_at :: UTCTime  ,
    comment_image     :: ImageId  ,
    comment_user      :: Username ,
    comment_body      :: String   ,
    comment_deleted   :: Bool
} | NullComment deriving (Show)

data User = User {
    user_id            :: UserId ,
    user_role          :: String ,
    user_created_at    :: UTCTime,
    user_comment_count :: Int    ,
    user_upload_count  :: Int    ,
    user_awards        :: [Award],
    user_name          :: String ,
    user_description   :: Maybe String
} | AnonymousUser | NullUser deriving (Show)

data Award = Award {
    award_id    :: AwardId,
    award_title :: String ,
    award_label :: String ,
    award_date  :: UTCTime
} | NullAward deriving (Show)

-- Classes

class Nullable a where
    null :: a

class (Show a) => Print a where
    toString :: a -> String

-- Instances

instance FromJSON ImageData where
    parseJSON o = do
        eImage <- eitherP (parseJSON o) (parseJSON o)
        case eImage of
            Left imageRegular    -> return $ ImageData imageRegular
            Right imageDuplicate -> return $ ImageDuplicateData imageDuplicate

instance FromJSON DuplicateData where
    parseJSON = withObject "duplicatedata" $ \o -> do
        i             <- o .:  "id"
        created_at    <- o .:  "created_at"
        updated_at    <- o .:  "updated_at"
        first_seen_at <- o .:  "first_seen_at"
        uploader      <- o .:? "uploader_id"
        duplicate_of  <- o .:  "duplicate_of"
        return DuplicateData {
            duplicate_image_id      = i,
            duplicate_created_at    = created_at,
            duplicate_updated_at    = updated_at,
            duplicate_first_seen_at = first_seen_at,
            duplicate_of_id         = duplicate_of,
            duplicate_uploader_id   = uploader
        }

instance FromJSON Data where
    parseJSON = withObject "data" $ \o -> do
        i             <- o .:  "id"
        upvotes       <- o .:  "upvotes"
        downvotes     <- o .:  "downvotes"
        faves         <- o .:  "faves"
        score         <- o .:  "score"
        height        <- o .:  "height"
        width         <- o .:  "width"
        aspect_ratio  <- o .:  "aspect_ratio"
        description   <- o .:  "description"
        created_at    <- o .:  "created_at"
        updated_at    <- o .:  "updated_at"
        first_seen_at <- o .:  "first_seen_at"
        tags          <- o .:  "tag_ids"
        uploader      <- o .:? "uploader_id"
        comment_count <- o .:  "comment_count"
        return Data {
            image_id            = i,
            image_upvotes       = upvotes,
            image_downvotes     = downvotes,
            image_faves         = faves,
            image_score         = score,
            image_height        = height,
            image_width         = width,
            image_aspect_ratio  = aspect_ratio,
            image_description   = description,
            image_created_at    = created_at,
            image_updated_at    = updated_at,
            image_first_seen_at = first_seen_at,
            image_tags          = tags,
            image_uploader      = uploader,
            image_comment_count = comment_count
        }

instance FromJSON SearchPage where
    parseJSON = withObject "searchpage" $ \o -> do
        search       <- o .: "search"
        total_count  <- o .: "total"
        return $ SearchPage total_count search

instance FromJSON CommentPage where
    parseJSON = withObject "commentspage" $ \o -> do
        comments <- o .: "comments"
        return $ CommentPage comments

instance FromJSON Comment where
    parseJSON = withObject "comment" $ \o -> do
        i         <- o .: "id"
        body      <- o .: "body"
        author    <- o .: "author"
        image     <- o .: "image_id"
        posted_at <- o .: "posted_at"
        deleted   <- o .: "deleted"
        return Comment {
            comment_id        = i,
            comment_body      = body,
            comment_user      = author,
            comment_image     = image,
            comment_posted_at = posted_at,
            comment_deleted   = deleted
        }

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        awards        <- o .:  "awards"
        i             <- o .:  "id"
        name          <- o .:  "name"
        role          <- o .:  "role"
        description   <- o .:? "description"
        created_at    <- o .:  "created_at"
        comment_count <- o .:  "comment_count"
        upload_count  <- o .:  "uploads_count"
        return $ User {
            user_id            = i,
            user_name          = name,
            user_description   = description,
            user_role          = role,
            user_created_at    = created_at,
            user_upload_count  = upload_count,
            user_comment_count = comment_count,
            user_awards        = awards
        }

instance FromJSON Award where
    parseJSON = withObject "award" $ \o -> do
        i          <- o .: "id"
        title      <- o .: "title"
        awarded_on <- o .: "awarded_on"
        label      <- o .: "label"
        return $ Award {
            award_id    = i,
            award_title = title,
            award_label = label,
            award_date  = awarded_on
        }

instance FromJSON Tag where
    parseJSON = withObject "tag" $ \o -> do
        i                 <- o .:  "id"
        name              <- o .:  "name"
        slug              <- o .:  "slug"
        description       <- o .:  "description"
        short_description <- o .:  "short_description"
        aliased_to        <- o .:? "aliased_to_id"
        implied_tags      <- o .:  "implied_tag_ids"
        return $ Tag {
            tag_id                = i,
            tag_name              = name,
            tag_slug              = slug,
            tag_description       = description,
            tag_short_desctiption = short_description,
            tag_aliased_to        = aliased_to,
            tag_implied_tags      = implied_tags
        }

instance FromJSON Settings where
    parseJSON (Object v) = 
        Settings
            <$> v .: "key"
            <*> v .: "images_per_page"
    parseJSON _ = fail "Unable to parse database credentials"

instance Nullable ImageData where
    null = ImageNullData
instance Nullable Comment where
    null = NullComment
instance Nullable CommentPage where
    null = NullCommentPage
instance Nullable SearchPage where
    null = NullSearchPage
instance Nullable User where
    null = NullUser
instance Nullable Award where
    null = NullAward
instance Nullable Tag where
    null = NullTag
instance (Nullable a) => Nullable [a] where
    null = [Datas.null]

instance {-# OVERLAPPING #-} Print String where
    toString = id
instance (Show a) => Print a where
    toString = show
