module DataManipulation where

import Datas
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

getImageId :: Image -> ImageId
getImageId (Image i)          = image_id i
getImageId (ImageDuplicate i) = duplicate_image_id i
getImageId _                        = 0

getSearchImages :: SearchPage -> [Image]
getSearchImages (SearchPage _ i) = i
getSearchImages _                = []

decodeNoMaybe :: (Nullable a, FromJSON a) => BL.ByteString -> a
decodeNoMaybe s =
    case decoded of
        Just a  -> a
        Nothing -> Datas.null
    where decoded = decode s