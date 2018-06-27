module DataHelpers where

import Datas
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Control.Concurrent.STM

getImageId :: Image -> ImageId
getImageId (Image i)          = image_id i
getImageId (DuplicateImage i) = duplicate_image_id i
getImageId (DeletedImage i)   = deleted_image_id i
getImageId _                  = 0

getSearchImages :: SearchPage -> [Image]
getSearchImages (SearchPage _ i) = i
getSearchImages _                = []

getTagPageTags :: TagPage -> [Tag]
getTagPageTags (TagPage t) = t
getTagPageTags _           = []

filterNulls :: (Nullable a) => [a] -> [a]
filterNulls = filter $ not . isnull

writeOut :: (Show a) => AppContext -> a -> IO ()
writeOut ctx o = atomically $ writeTQueue (schedOut $ app_sched ctx) $ show o

decodeNoMaybe :: (Nullable a, FromJSON a) => ByteString -> a
decodeNoMaybe s =
    case decoded of
        Just a  -> a
        Nothing -> Datas.null
    where decoded = decode s
