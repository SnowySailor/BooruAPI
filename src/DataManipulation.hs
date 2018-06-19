module DataManipulation where

import Datas
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

getImageId :: ImageData -> ImageId
getImageId (ImageData d) = image_id d
getImageId _             = 0

getSearchImages :: SearchPage -> [ImageData]
getSearchImages (SearchPage _ i) = i
getSearchImages _                = []

decodeNoMaybe :: (Nullable a, FromJSON a) => BL.ByteString -> a
decodeNoMaybe s =
    case decoded of
        Just a  -> a
        Nothing -> Datas.null
    where decoded = decode s