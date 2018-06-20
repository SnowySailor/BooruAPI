{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Datas where

import Control.Concurrent
import qualified Data.Map as M
import Database.PostgreSQL.Simple
import Data.Pool
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

data Settings    = Settings String Int Int deriving (Show)
data CommentPage = CommentPage [Comment] | NullCommentPage deriving (Show)
data SearchPage  = SearchPage Int [Image] | NullSearchPage deriving (Show)
data Image       = Image ImageData | ImageDuplicate DuplicateData | NullImage deriving (Show)
data ImageFull   = ImageFull ImageData [Comment] | ImageDuplicateFull DuplicateData | NullImageFull deriving (Show)
data User        = User UserData | AnonymousUser | NullUser deriving (Show)
data UserFull    = UserFull UserData [ImageId] | AnonymousUserFull | NullUserFull deriving (Show)

data ImageData = ImageData {
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
} | NullImageData deriving (Show)

data DuplicateData = DuplicateData {
    duplicate_image_id      :: Int      ,
    duplicate_created_at    :: UTCTime  ,
    duplicate_updated_at    :: UTCTime  ,
    duplicate_first_seen_at :: UTCTime  ,
    duplicate_uploader_id   :: Maybe Int,
    duplicate_of_id         :: Int      
} | NullDuplicateData deriving (Show)

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

data UserData = UserData {
    user_id            :: UserId ,
    user_role          :: String ,
    user_created_at    :: UTCTime,
    user_comment_count :: Int    ,
    user_upload_count  :: Int    ,
    user_awards        :: [Award],
    user_name          :: String ,
    user_description   :: Maybe String
} | NullUserData deriving (Show)

data Award = Award {
    award_id    :: AwardId,
    award_title :: String ,
    award_label :: String ,
    award_date  :: UTCTime
} | NullAward deriving (Show)

data DatabaseCredentials = DatabaseCredentials {
    db_user     :: String,
    db_password :: String
} deriving (Show)

data ServerResources = ServerResources {
    serverPools    :: MVar (M.Map String (Pool Connection)),
    serverPoolLock :: MVar () -- write lock
}

-- Classes

class Nullable a where
    null :: a

class (Show a) => Print a where
    toString :: a -> String

-- Instances

instance FromJSON Image where
    parseJSON o = do
        eImage <- eitherP (parseJSON o) (parseJSON o)
        case eImage of
            Left imageRegular    -> return $ Image imageRegular 
            Right imageDuplicate -> return $ ImageDuplicate imageDuplicate

instance FromJSON User where
    parseJSON o = do
        userD <- parseJSON o
        return $ User userD

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

instance FromJSON ImageData where
    parseJSON (Object o) =
        ImageData
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
            <$> o .: "total"
            <*> o .: "search"
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

instance FromJSON UserData where
    parseJSON (Object o) =
        UserData
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

instance FromJSON DatabaseCredentials where
    parseJSON (Object v) = 
        DatabaseCredentials <$> (v .: "user")
                            <*> (v .: "password")
    parseJSON _          = fail "Unable to parse non-Object"

instance Nullable Image where
    null = NullImage
instance Nullable ImageFull where
    null = NullImageFull
instance Nullable ImageData where
    null = NullImageData
instance Nullable DuplicateData where
    null = NullDuplicateData
instance Nullable Comment where
    null = NullComment
instance Nullable CommentPage where
    null = NullCommentPage
instance Nullable SearchPage where
    null = NullSearchPage
instance Nullable UserData where
    null = NullUserData
instance Nullable Award where
    null = NullAward
instance Nullable Tag where
    null = NullTag
instance Nullable User where
    null = NullUser
instance (Nullable a) => Nullable [a] where
    null = [Datas.null]

instance {-# OVERLAPPING #-} Print String where
    toString = id
instance (Show a) => Print a where
    toString = show
