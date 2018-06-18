{-# LANGUAGE OverloadedStrings #-}
module Datas where

import Data.ByteString (ByteString)
import Data.Dates
import Data.Aeson
import Data.Time.Clock
import Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.ByteString

import Helpers
import Types
import APIGetter

data Award = Award {
    award_id    :: AwardId   ,
    award_title :: ByteString,
    award_label :: ByteString,
    award_date  :: UTCTime
} | NullAward deriving (Show)

data Tag = Tag {
    tag_id                :: TagId     ,
    tag_name              :: ByteString,
    tag_short_desctiption :: ByteString,
    tag_description       :: ByteString,
    tag_implied_tags      :: [TagId]
} | NullTag deriving (Show)

data CommentPage = CommentPage [Comment] | NullCommentPage deriving (Show)

data Comment = Comment {
    comment_id        :: CommentId ,
    comment_posted_at :: UTCTime   ,
    comment_image     :: ImageId   ,
    comment_user      :: Username  ,
    comment_body      :: String,
    comment_deleted   :: Bool
} | NullComment deriving (Show)

data Image = Image ImageData [Comment] deriving (Show)
data ImageData = ImageData Data | ImageDuplicateData DuplicateData | ImageNullData deriving (Show)

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
    image_tags          :: [TagId]     ,
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

data User = User {
    user_id            :: UserId    ,
    user_role          :: ByteString,
    user_created_at    :: UTCTime   ,
    user_comment_count :: Int       ,
    user_upload_count  :: Int       ,
    user_awards        :: [AwardId]   ,
    user_name          :: ByteString,
    user_favorites     :: [ImageId]
} | AnonymousUser | NullUser deriving (Show)

instance FromJSON ImageData where
    parseJSON o = do
        eImage <- eitherP (parseJSON o) (parseJSON o)
        case eImage of
            Left imageRegular    -> return $ ImageData imageRegular
            Right imageDuplicate -> return $ ImageDuplicateData imageDuplicate

instance FromJSON DuplicateData where
    parseJSON = withObject "duplicatedata" $ \o -> do
        id            <- o .:  "id"
        created_at    <- o .:  "created_at"
        updated_at    <- o .:  "updated_at"
        first_seen_at <- o .:  "first_seen_at"
        uploader      <- o .:? "uploader_id"
        duplicate_of  <- o .:  "duplicate_of"
        return DuplicateData {
            duplicate_image_id      = id,
            duplicate_created_at    = parseJSONTime created_at,
            duplicate_updated_at    = parseJSONTime updated_at,
            duplicate_first_seen_at = parseJSONTime first_seen_at,
            duplicate_of_id         = duplicate_of,
            duplicate_uploader_id   = uploader
        }

instance FromJSON Data where
    parseJSON = withObject "data" $ \o -> do
        id            <- o .:  "id"
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
            image_id            = id,
            image_upvotes       = upvotes,
            image_downvotes     = downvotes,
            image_faves         = faves,
            image_score         = score,
            image_height        = height,
            image_width         = width,
            image_aspect_ratio  = aspect_ratio,
            image_description   = description,
            image_created_at    = parseJSONTime created_at,
            image_updated_at    = parseJSONTime updated_at,
            image_first_seen_at = parseJSONTime first_seen_at,
            image_tags          = tags,
            image_uploader      = uploader,
            image_comment_count = comment_count
        }


instance FromJSON CommentPage where
    parseJSON = withObject "CommentPage" $ \o -> do
        comments <- o .: "comments"
        return $ CommentPage comments

instance FromJSON Comment where
    parseJSON = withObject "comment" $ \o -> do
        id        <- o .: "id"
        body      <- o .: "body"
        author    <- o .: "author"
        image_id  <- o .: "image_id"
        posted_at <- o .: "posted_at"
        deleted   <- o .: "deleted"
        return Comment {
            comment_id        = id,
            comment_body      = body,
            comment_user      = author,
            comment_image     = image_id,
            comment_posted_at = posted_at,
            comment_deleted   = deleted
        }

class Nullable a where
    null :: a
instance Nullable ImageData where
    null = ImageNullData
instance Nullable Comment where
    null = NullComment
instance (Nullable a) => Nullable [a] where
    null = [Datas.null]
instance Nullable CommentPage where
    null = NullCommentPage

getImage :: ImageId -> IO Image
getImage i = do
    imageData <- getImageData i
    comments  <- case imageData of
                    ImageData d          -> getImageComments i (image_comment_count d)
                    ImageDuplicateData _ -> return []
                    ImageNullData        -> return []
    return $ Image imageData comments

getImageData :: ImageId -> IO ImageData
getImageData i = do
    imageData <- getImageJSON i
    return $ decodeNoMaybe imageData

getImageComments :: ImageId -> Int -> IO [Comment]
getImageComments i count = do
    comments <- mapM (getCommentPage i) [1..(p+1)]
    return $ flatten $ map (\(CommentPage c) -> c) comments
    where (p, _) = divMod count 20

getCommentPage :: ImageId -> PageNo -> IO CommentPage
getCommentPage i p = do
    json <- getCommentsJSON i p
    return $ decodeNoMaybe json

decodeNoMaybe :: (Nullable a, FromJSON a) => BL.ByteString -> a
decodeNoMaybe s =
    case decoded of
        Just a  -> a
        Nothing -> Datas.null
    where decoded = decode s





