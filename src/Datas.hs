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

data Settings          = Settings String Int Int deriving (Show)
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
    duplicate_image_id      :: Int      ,
    duplicate_created_at    :: UTCTime  ,
    duplicate_updated_at    :: UTCTime  ,
    duplicate_first_seen_at :: UTCTime  ,
    duplicate_uploader_id   :: Maybe Int,
    duplicate_of_id         :: Int      
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
    comment_author    :: Username ,
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
    parseJSON (Object o) =
        DuplicateData
            <$> o .:  "id"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
            <*> o .:? "uploader_id"
            <*> o .:  "duplicate_of"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON Data where
    parseJSON (Object o) =
        Data
            <$> o .:  "id"
            <*> o .:  "upvotes"
            <*> o .:  "downvotes"
            <*> o .:  "faves"
            <*> o .:  "score"
            <*> o .:  "height"
            <*> o .:  "width"
            <*> o .:  "aspect_ratio"
            <*> o .:  "description"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
            <*> o .:  "tag_ids"
            <*> o .:? "uploader_id"
            <*> o .:  "comment_count"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON SearchPage where
    parseJSON (Object o) =
        SearchPage
            <$> o .: "search"
            <*> o .: "total"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON CommentPage where
    parseJSON (Object o) =
        CommentPage <$> o .: "comments"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON Comment where
    parseJSON (Object o) =
        Comment
            <$> o .: "id"
            <*> o .: "posted_at"
            <*> o .: "image_id"
            <*> o .: "author"
            <*> o .: "body"
            <*> o .: "deleted"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON User where
    parseJSON (Object o) =
        User
            <$> o .:  "id"
            <*> o .:  "role"
            <*> o .:  "created_at"
            <*> o .:  "comment_count"
            <*> o .:  "uploads_count"
            <*> o .:  "awards"
            <*> o .:  "name"
            <*> o .:? "description"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON Award where
    parseJSON (Object o) =
        Award
            <$> o .: "id"
            <*> o .: "title"
            <*> o .: "label"
            <*> o .: "awarded_on"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON Tag where
    parseJSON (Object o) =
        Tag
            <$> o .:  "id"
            <*> o .:  "name"
            <*> o .:  "slug"
            <*> o .:  "short_description"
            <*> o .:  "description"
            <*> o .:  "implied_tag_ids"
            <*> o .:? "aliased_to_id"
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON Settings where
    parseJSON (Object v) = 
        Settings
            <$> v .:  "key"
            <*> v .:? "images_per_page" .!= 50
            <*> v .:? "comments_per_page" .!= 20
    parseJSON _          = fail "Unable to parse non-Object"

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
