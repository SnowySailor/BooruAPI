{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module Datas where

import Control.Applicative
import Control.Concurrent
import qualified Data.Map as M
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
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
data TagPage     = TagPage [Tag] | NullTagPage deriving (Show)
data Image       = Image ImageData | ImageDuplicate DuplicateData | ImageDeleted DeletedData | NullImage deriving (Show)
data ImageFull   = ImageFull ImageData [Comment] | ImageDuplicateFull DuplicateData | ImageDeletedFull DeletedData | NullImageFull deriving (Show)
data User        = User UserData | AnonymousUser | NullUser deriving (Show)
data UserFull    = UserFull UserData [ImageId] | AnonymousUserFull | NullUserFull deriving (Show)

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
} | NullImageData deriving (Show)

data DuplicateData = DuplicateData {
    duplicate_image_id      :: Int      ,
    duplicate_of_id         :: Int      ,
    duplicate_uploader_id   :: Maybe Int,
    duplicate_created_at    :: UTCTime  ,
    duplicate_updated_at    :: UTCTime  ,
    duplicate_first_seen_at :: UTCTime
} | NullDuplicateData deriving (Show)

data DeletedData = DeletedData {
    deleted_image_id      :: Int      ,
    deleted_uploader_id   :: Maybe Int,
    deleted_reason        :: String   ,
    deleted_created_at    :: UTCTime  ,
    deleted_updated_at    :: UTCTime  ,
    deleted_first_seen_at :: UTCTime
} | NullDeletedData deriving (Show)

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

data UserData = UserData {
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
} | NullUserData deriving (Show)

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
            Left imageRegular           -> Image imageRegular 
            Right eImage2 -> 
                case eImage2 of
                    Left imageDuplicate -> ImageDuplicate imageDuplicate
                    Right imageDeleted  -> ImageDeleted imageDeleted
    parseJSON _          = pure NullImage

instance FromJSON User where
    parseJSON o = do
        userD <- parseJSON o
        return $ User userD

instance FromJSON DeletedData where
    parseJSON (Object o) =
        DeletedData
            <$> o .:  "id"
            <*> o .:? "uploader_id"
            <*> o .:  "deletion_reason"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
        <|> pure NullDeletedData
    parseJSON _          = pure NullDeletedData

instance FromJSON DuplicateData where
    parseJSON (Object o) =
        DuplicateData
            <$> o .:  "id"
            <*> o .:  "duplicate_of"
            <*> o .:? "uploader_id"
            <*> o .:  "created_at"
            <*> o .:  "updated_at"
            <*> o .:  "first_seen_at"
        <|> pure NullDuplicateData
    parseJSON _          = pure NullDuplicateData

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
        <|> pure NullImageData
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

instance FromJSON UserData where
    parseJSON (Object o) =
        UserData
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
        <|> pure NullUserData
    parseJSON _          = pure NullUserData

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
            <*> v .:? "images_per_page" .!= 50
            <*> v .:? "comments_per_page" .!= 20
    parseJSON _          = fail "Unable to parse non-Object"

instance FromJSON DatabaseCredentials where
    parseJSON (Object v) = 
        DatabaseCredentials
            <$> (v .: "user")
            <*> (v .: "password")
    parseJSON _          = fail "Unable to parse non-Object"

instance Nullable Image where
    null = NullImage
    isnull (NullImage) = True
instance Nullable ImageFull where
    null = NullImageFull
    isnull (NullImageFull) = True
    isnull _               = False
instance Nullable ImageData where
    null = NullImageData
    isnull (NullImageData) = True
    isnull _               = False
instance Nullable DuplicateData where
    null = NullDuplicateData
    isnull (NullDuplicateData) = True
    isnull _                   = False
instance  Nullable DeletedData where
    null = NullDeletedData
    isnull (NullDeletedData) = True
    isnull _                 = False
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
instance Nullable UserData where
    null = NullUserData
    isnull (NullUserData) = True
    isnull _              = False
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
