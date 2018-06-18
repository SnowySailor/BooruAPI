{-# LANGUAGE OverloadedStrings #-}
module Datas where

import Data.ByteString (ByteString)
import Data.Dates
import Data.Aeson
import Data.Time.Clock
import Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Helpers
 
testData = BL.fromStrict $ T.encodeUtf8 "{\"id\":828677,\"created_at\":\"2015-02-14T23:26:04.741Z\",\"updated_at\":\"2018-06-17T13:13:06.932Z\",\"first_seen_at\":\"2015-02-14T23:26:04.741Z\",\"score\":849,\"comment_count\":200000,\"width\":705,\"height\":810,\"file_name\":\"00_26_04_731_file\",\"description\":\"An allusion to \\u003e\\u003e827454. When this image was featured on Derpibooru, its comment box was derailed by 4chan haters into a Skeletor thread.\",\"uploader\":\"SlayerBVC\",\"uploader_id\":211879,\"image\":\"//derpicdn.net/img/view/2015/2/14/828677__safe_artist-colon-dm29_flash+sentry_twilight+sparkle_adventure+in+the+comments_alicorn_blushing_bouquet_box_box+of+chocolates_brony+history_car.png\",\"upvotes\":1174,\"downvotes\":325,\"faves\":805,\"tags\":\"adventure in the comments, alicorn, artist:dm29, blushing, bouquet, box, box of chocolates, brony history, carrying, comment event horizon, comments locked down, comments more entertaining, cute, derail in the comments, derpibooru history, derpibooru legacy, diasentres, eternal thread, featured image, female, flashlight, flash sentry, flower, flower in hair, folded wings, food, happy, heart, hearts and hooves day, hoof hold, julian yeo is trying to murder us, legendary, letter, lilacs, looking back, looking up, male, mare, mouth hold, pegasus, ponies riding ponies, pony, raised hoof, riding, safe, shipping, signature, simple background, skeletor in the comments, smiling, song in the comments, spread wings, stallion, straight, the eternal thread, the former eternal thread, the image formerly known as the eternal thread, the image that started zeb's eternal feud with sirbumpaous, thread war, transparent background, trotting, twiabetes, twilight sparkle, twilight sparkle (alicorn), valentine card, valentine's day, vector, wall of tags, wings, you guys are awesome and i love you\",\"tag_ids\":[40482,245120,46631,46192,45978,45806,45249,43277,187857,182100,41769,41508,39938,27475,82531,24859,27572,27578,21297,38165,61247,5482,22401,105582,21497,35219,122116,89950,191172,29860,73916,27821,80859,24249,190054,666,38185,37319,60946,33983,42939,27141,33855,41700,29630,189561,39318,445,21521,32938,115727,319526,46705,33288,321220,47596,29837,255450,339483,66853,186940,378063,102176,333251,333249,333250,333248,335106,333207,216960],\"aspect_ratio\":0.87037037037037,\"original_format\":\"png\",\"mime_type\":\"image/png\",\"sha512_hash\":\"f0e52364918085db73fb4fb0f6b0ebb4b2fdfa142fbf47eae5b03faa99771909e34e039fd1493da476e333cd6e8f67449d6a58876a13ea6ed33faadb73bd2ac1\",\"orig_sha512_hash\":\"f0e52364918085db73fb4fb0f6b0ebb4b2fdfa142fbf47eae5b03faa99771909e34e039fd1493da476e333cd6e8f67449d6a58876a13ea6ed33faadb73bd2ac1\",\"source_url\":\"http://dm29.deviantart.com/art/I-Feel-Like-A-Princess-513854102\",\"representations\":{\"thumb_tiny\":\"//derpicdn.net/img/2015/2/14/828677/thumb_tiny.png\",\"thumb_small\":\"//derpicdn.net/img/2015/2/14/828677/thumb_small.png\",\"thumb\":\"//derpicdn.net/img/2015/2/14/828677/thumb.png\",\"small\":\"//derpicdn.net/img/2015/2/14/828677/small.png\",\"medium\":\"//derpicdn.net/img/2015/2/14/828677/medium.png\",\"large\":\"//derpicdn.net/img/2015/2/14/828677/large.png\",\"tall\":\"//derpicdn.net/img/2015/2/14/828677/tall.png\",\"full\":\"//derpicdn.net/img/view/2015/2/14/828677__safe_artist-colon-dm29_flash+sentry_twilight+sparkle_adventure+in+the+comments_alicorn_blushing_bouquet_box_box+of+chocolates_brony+history_car.png\"},\"is_rendered\":true,\"is_optimized\":true,\"interactions\":[]}"

data Award = Award {
    award_id    :: Int       ,
    award_title :: ByteString,
    award_label :: ByteString,
    award_date  :: UTCTime
} deriving (Show)

data Tag = Tag {
    tag_id                :: Int       ,
    tag_name              :: ByteString,
    tag_short_desctiption :: ByteString,
    tag_description       :: ByteString,
    tag_implied_tags      :: [Tag]
} deriving (Show)

data Comment = Comment {
    comment_id        :: Int       ,
    comment_posted_at :: UTCTime  ,
    comment_image     :: Image     ,
    comment_user      :: User      ,
    comment_body      :: ByteString,
    comment_deleted   :: Bool
} deriving (Show)

data Image = Image {
    image_id            :: Int       ,
    image_upvotes       :: Int       ,
    image_downvotes     :: Int       ,
    image_faves         :: Int       ,
    image_score         :: Int       ,
    image_height        :: Int       ,
    image_width         :: Int       ,
    image_aspect_ratio  :: Double    ,
    image_description   :: String,
    image_created_at    :: UTCTime  ,
    image_updated_at    :: UTCTime  ,
    image_first_seen_at :: UTCTime  ,
    image_tags          :: [Int]    ,
    image_uploader      :: Int      ,
    image_comments      :: [Comment]
} deriving (Show)

data User = User {
    user_id            :: Int       ,
    user_role          :: ByteString,
    user_created_at    :: UTCTime  ,
    user_comment_count :: Int       ,
    user_upload_count  :: Int       ,
    user_awards        :: [Award]   ,
    user_name          :: ByteString,
    user_favorites     :: [Image]
} | AnonymousUser deriving (Show)

instance FromJSON Image where
    parseJSON = withObject "image" $ \o -> do
        id            <- o .: "id"
        upvotes       <- o .: "upvotes"
        downvotes     <- o .: "downvotes"
        faves         <- o .: "faves"
        score         <- o .: "score"
        height        <- o .: "height"
        width         <- o .: "width"
        aspect_ratio  <- o .: "aspect_ratio"
        description   <- o .: "description"
        created_at    <- o .: "created_at"
        updated_at    <- o .: "updated_at"
        first_seen_at <- o .: "first_seen_at"
        tags          <- o .: "tag_ids"
        uploader      <- o .: "uploader_id"
        comment_count <- o .: "comment_count"
        let comments  = getImageComments id comment_count
        return Image {
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
            image_comments      = comments
        }

getImageComments :: Int -> Int -> [Comment]
getImageComments i c =
    flatten $ map (getCommentPage i) [1..(p+1)]
    where (p, _) = divMod c 50

getCommentPage :: Int -> Int -> [Comment]
getCommentPage i p = undefined
