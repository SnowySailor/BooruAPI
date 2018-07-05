{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Datas where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Aeson
import Data.Time.Clock
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy (ByteString)
import Data.HashSet
import GHC.Word

-- Types

type PageNo      = Int
type ImageId     = Int
type CommentId   = Int
type TagId       = Int
type AwardId     = Int
type UserId      = Int
type Username    = String
type RateLimiter = TBQueue ()

-- Datas

data CommentPage = CommentPage [Comment] | NullCommentPage deriving (Show)
data SearchPage  = SearchPage Int [Image] | NullSearchPage deriving (Show)
data TagPage     = TagPage [Tag] | NullTagPage deriving (Show)
data Image       = Image ImageData | DuplicateImage DuplicateImageData | DeletedImage DeletedImageData | NullImage deriving (Show)
data HTTPMethod  = GET | POST deriving (Show)

data ImageData = ImageData {
    image_id            :: ImageId   ,
    image_uploader_id   :: Maybe Int ,
    image_description   :: String    ,
    image_upvotes       :: Int       ,
    image_downvotes     :: Int       ,
    image_faves         :: Int       ,
    image_score         :: Int       ,
    image_tags          :: [TagId]   ,
    image_comment_count :: Int       ,
    image_created_at    :: UTCTime   ,
    image_updated_at    :: UTCTime   ,
    image_first_seen_at :: UTCTime   ,
    image_height        :: Int       ,
    image_width         :: Int       ,
    image_aspect_ratio  :: Double
} | NullImageData deriving (Show, Eq)

data DuplicateImageData = DuplicateImageData {
    duplicate_image_id      :: Int      ,
    duplicate_of_id         :: Int      ,
    duplicate_uploader_id   :: Maybe Int,
    duplicate_created_at    :: UTCTime  ,
    duplicate_updated_at    :: UTCTime  ,
    duplicate_first_seen_at :: UTCTime
} | NullDuplicateImageData deriving (Show, Eq)

data DeletedImageData = DeletedImageData {
    deleted_image_id      :: Int      ,
    deleted_uploader_id   :: Maybe Int,
    deleted_reason        :: String   ,
    deleted_created_at    :: UTCTime  ,
    deleted_updated_at    :: UTCTime  ,
    deleted_first_seen_at :: UTCTime
} | NullDeletedImageData deriving (Show, Eq)

data Tag = Tag {
    tag_id                :: TagId       ,
    tag_name              :: String      ,
    tag_slug              :: String      ,
    tag_description       :: String      ,
    tag_short_desctiption :: String      ,
    tag_aliased_to        :: Maybe TagId ,
    tag_implied_tags      :: [TagId]     ,
    tag_category          :: Maybe String,
    tag_spoiler_image     :: Maybe String
} | NullTag deriving (Show)

data Comment = Comment {
    comment_id        :: CommentId,
    comment_image_id  :: ImageId  ,
    comment_author    :: Username ,
    comment_body      :: String   ,
    comment_posted_at :: UTCTime  ,
    comment_deleted   :: Bool
} | NullComment deriving (Show)

data User = User {
    user_id            :: UserId      ,
    user_name          :: String      ,
    user_description   :: Maybe String,
    user_role          :: String      ,
    user_created_at    :: UTCTime     ,
    user_comment_count :: Int         ,
    user_upload_count  :: Int         ,
    user_post_count    :: Int         ,
    user_topic_count   :: Int         ,
    user_awards        :: [Award]     ,
    user_links         :: [Link]
} | AnonymousUser | NullUser deriving (Show)

data Award = Award {
    award_id    :: AwardId,
    award_title :: String ,
    award_label :: String ,
    award_date  :: UTCTime
} | NullAward deriving (Show)

data Link = Link {
    link_user_id    :: Int,
    link_tag_id     :: Int,
    link_created_at :: UTCTime,
    link_state      :: String
} | NullLink deriving (Show)

data RequestQueues = RequestQueues {
    requestQueuesMain  :: TBQueue QueueRequest,
    requestQueuesRetry :: TQueue QueueRequest,
    requestRateLimiter :: Maybe RateLimiter,
    requestInProgress  :: TMVar Int
}

data QueueResponse = QueueResponse {
    queueResponseBody   :: ByteString,
    queueResponseStatus :: Int
}

data QueueRequest = QueueRequest {
    requestUri      :: String,
    requestBody     :: Maybe ByteString,
    requestMethod   :: HTTPMethod,
    requestTries    :: Int,
    requestCodes    :: [Int],
    requestTriesMax :: Int,
    requestCallback :: RequestQueues -> QueueRequest -> QueueResponse -> IO ()
}

data DatabaseCredentials = DatabaseCredentials {
    db_host     :: String,
    db_port     :: Word16,
    db_user     :: String,
    db_password :: String,
    db_database :: String,
    db_schema   :: String
} deriving (Show)

data ServerResources = ServerResources {
    serverPools    :: MVar (Map String (Pool Connection)),
    serverPoolLock :: MVar () -- write lock
}

data Settings = Settings {
    api_key             :: String,
    images_per_page     :: Int,
    comments_per_page   :: Int,
    load_image_start    :: Int,
    load_image_end      :: Int,
    load_user_start     :: Int,
    load_user_end       :: Int,
    load_full_images    :: Bool,
    load_full_users     :: Bool,
    num_request_threads :: Int,
    requests_per_second :: Double,
    max_retry_count     :: Int
} deriving (Show)

data Scheduler = Scheduler {
    schedRateLimiter :: TBQueue (),
    schedReqPerSec   :: Double,
    schedOut         :: TQueue String
}

-- Classes

class Nullable a where
    null :: a
    isnull :: a -> Bool

class Validatable a where
    invalid :: a
    isvalid :: a -> Bool

class (Show a) => Print a where
    toString :: a -> String

-- Instances

instance FromJSON Image where
    parseJSON (Object o) = do
        let obj = (Object o)
        eImage <- eitherP (parseJSON obj) $ eitherP (parseJSON obj) (parseJSON obj)
        return $ case eImage of
            Left imageRegular           -> if imageRegular == NullImageData then NullImage else Image imageRegular
            Right eImage2 ->
                case eImage2 of
                    Left imageDuplicate -> if imageDuplicate == NullDuplicateImageData then NullImage else DuplicateImage imageDuplicate
                    Right imageDeleted  -> if imageDeleted == NullDeletedImageData then NullImage else DeletedImage imageDeleted
    parseJSON _          = pure NullImage

instance FromJSON DeletedImageData where
    parseJSON (Object o) =
        DeletedImageData
            <$> o .:  "id"
            <*> o .:? "uploader_id"
            <*> o .:  "deletion_reason"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
    parseJSON _          = pure NullDeletedImageData

instance FromJSON DuplicateImageData where
    parseJSON (Object o) =
        DuplicateImageData
            <$> o .:  "id"
            <*> o .:  "duplicate_of"
            <*> o .:? "uploader_id"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
    parseJSON _          = pure NullDuplicateImageData

instance FromJSON ImageData where
    parseJSON (Object o) =
        ImageData
            <$> o .:  "id"
            <*> o .:? "uploader_id"
            <*> o .:  "description"
            <*> o .:  "upvotes"
            <*> o .:  "downvotes"
            <*> o .:  "faves"
            <*> o .:  "score"
            <*> o .:  "tag_ids"
            <*> o .:  "comment_count"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
            <*> o .:  "height"
            <*> o .:  "width"
            <*> o .:  "aspect_ratio"
    parseJSON _          = pure NullImageData

instance FromJSON SearchPage where
    parseJSON (Object o) =
        SearchPage
            <$> o .: "total"
            <*> o .: "search"
        <|> pure NullSearchPage
    parseJSON _          = pure NullSearchPage

instance FromJSON CommentPage where
    parseJSON (Object o) =
        CommentPage <$> o .: "comments"
        <|> pure NullCommentPage
    parseJSON _          = pure NullCommentPage

instance FromJSON TagPage where
    parseJSON (Object o) = do
        tags <- parseJSON (Object o)
        return $ case tags of
            Just t  -> TagPage t
            Nothing -> NullTagPage
    parseJSON _          = pure NullTagPage

instance FromJSON Comment where
    parseJSON (Object o) =
        Comment
            <$> o .: "id"
            <*> o .: "image_id"
            <*> o .: "author"
            <*> o .: "body"
            <*> o .: "posted_at"
            <*> o .: "deleted"
        <|> pure NullComment
    parseJSON _          = pure NullComment

instance FromJSON User where
    parseJSON (Object o) =
        User
            <$> o .:  "id"
            <*> o .:  "name"
            <*> o .:? "description"
            <*> o .:  "role"
            <*> o .:  "created_at"
            <*> o .:  "comment_count"
            <*> o .:  "uploads_count"
            <*> o .:  "post_count"
            <*> o .:  "topic_count"
            <*> o .:  "awards"
            <*> o .:  "links"
        <|> pure NullUser
    parseJSON _          = pure NullUser

instance FromJSON Award where
    parseJSON (Object o) =
        Award
            <$> o .: "id"
            <*> o .: "title"
            <*> o .: "label"
            <*> o .: "awarded_on"
        <|> pure NullAward
    parseJSON _          = pure NullAward

instance FromJSON Link where
    parseJSON (Object o) =
        Link
            <$> o .: "user_id"
            <*> o .: "tag_id"
            <*> o .: "created_at"
            <*> o .: "state"
        <|> pure NullLink
    parseJSON _          = pure NullLink

instance FromJSON Tag where
    parseJSON (Object o) =
        Tag
            <$> o .:  "id"
            <*> o .:  "name"
            <*> o .:  "slug"
            <*> o .:  "description"
            <*> o .:  "short_description"
            <*> o .:? "aliased_to_id"
            <*> o .:  "implied_tag_ids"
            <*> o .:? "category"
            <*> o .:? "spoiler_image_uri"
        <|> pure NullTag
    parseJSON _          = pure NullTag

instance FromJSON Settings where
    parseJSON (Object v) = 
        Settings
            <$> v .:  "key"
            <*> v .:? "images_per_page"     .!= 50
            <*> v .:? "comments_per_page"   .!= 20
            <*> v .:? "start_image_id"      .!= 0
            <*> v .:? "end_image_id"        .!= 0
            <*> v .:? "start_user_id"       .!= 0
            <*> v .:? "end_user_id"         .!= 0
            <*> v .:? "load_full_images"    .!= False
            <*> v .:? "load_full_users"     .!= False
            <*> v .:? "num_request_threads" .!= 1
            <*> v .:? "max_requests_per_second" .!= 4.0
            <*> v .:? "max_retry_count"     .!= 2
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON DatabaseCredentials where
    parseJSON (Object v) = 
        DatabaseCredentials
            <$> v .:? "host"     .!= "0.0.0.0"
            <*> v .:? "port"     .!= 5432
            <*> v .:  "user"
            <*> v .:  "password"
            <*> v .:? "database" .!= "postgres"
            <*> v .:? "schema"   .!= "public"
    parseJSON _          = fail "Unable to parse non-Object"

instance Nullable Image where
    null = NullImage
    isnull (NullImage) = True
    isnull _           = False
instance Nullable Comment where
    null = NullComment
    isnull (NullComment) = True
    isnull _             = False
instance Nullable CommentPage where
    null = NullCommentPage
    isnull (NullCommentPage) = True
    isnull _                 = False
instance Nullable SearchPage where
    null = NullSearchPage
    isnull (NullSearchPage) = True
    isnull _                = False
instance Nullable Award where
    null = NullAward
    isnull (NullAward) = True
    isnull _           = False
instance Nullable Tag where
    null = NullTag
    isnull (NullTag) = True
    isnull _         = False
instance Nullable User where
    null = NullUser
    isnull (NullUser) = True
    isnull _          = False
instance Nullable TagPage where
    null = NullTagPage
    isnull (NullTagPage) = True
    isnull _             = False


instance {-# OVERLAPPING #-} Print String where
    toString = id
instance (Show a) => Print a where
    toString = show


instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n]
