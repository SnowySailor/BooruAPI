module DerpCloner where

import Helpers
import APIGetter
import Database.Loader
import Datas
import DataHelpers
import RequestQueues
import DerpAPI

import Data.Pool
import Control.Monad
import Control.Concurrent.STM.TQueue
import Database.PostgreSQL.Simple as P

handleImageResponse :: Settings -> Pool P.Connection -> String -> TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleImageResponse sett pool schema out rq req resp = do
    if status >= 200 && status < 300 then do
        case image of
            Image i -> do
                when (load_full_images sett) $ do
                    comments <- getImageComments' (image_id i) (image_comment_count i) sett out (requestRateLimiter rq)
                    loaded <- withResource pool $ \conn -> loadComments (flatten $ map getCommentsFromPage comments) conn schema
                    writeOut out $ show loaded
                loaded <- withResource pool $ \conn -> loadImage image conn schema out
                writeOut out $ show loaded
            DuplicateImage i -> do
                loaded <- withResource pool $ \conn -> loadImage image conn schema out
                writeOut out $ show loaded
            DeletedImage i -> do
                loaded <- withResource pool $ \conn -> loadImage image conn schema out
                writeOut out $ show loaded
            NullImage -> writeOut out $ "Null image at " ++ requestUri req
    else do
        writeOut out $ "Got " ++ show status ++ " at " ++ requestUri req
        handleBadResponse out rq req resp
    where image = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

handleUserResponse :: Settings -> Pool P.Connection -> String -> TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleUserResponse sett pool schema out rq req resp = do
    if status >= 200 && status < 300 then do
        case user of
            User{} -> do
                when (load_full_users sett) $ do
                    faves <- getUserFavorites' (user_name user) sett out (requestRateLimiter rq)
                    loaded <- withResource pool $ \conn -> loadUserFavorites (user_id user) faves conn schema out
                    writeOut out $ show loaded
                loaded <- withResource pool $ \conn -> loadUser user conn schema out
                writeOut out $ show loaded
            NullUser -> writeOut out $ "Null user at " ++ requestUri req
            _  -> writeOut out "Uh oh"
    else do
        writeOut out $ "Got " ++ show status ++ " at " ++ requestUri req
        handleBadResponse out rq req resp
    where user = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

handleTagPageResponse :: Settings -> Pool P.Connection -> String -> TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleTagPageResponse sett pool schema out rq req resp = do
    if status >= 200 && status < 300 then do
        case page of
            TagPage t -> do
                loaded <- withResource pool $ \conn -> loadTags t conn schema out
                writeOut out $ show loaded
            NullTagPage -> writeOut out $ "Null tag page at " ++ requestUri req ++ ": " ++ (show $ queueResponseBody resp)
    else do
        writeOut out $ "Got " ++ show status ++ " at " ++ requestUri req
        handleBadResponse out rq req resp
    where page = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

makeImageRequest :: Settings -> Pool P.Connection -> String -> TQueue String -> ImageId -> QueueRequest
makeImageRequest s p schema out i = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleImageResponse s p schema out
    where uri = imageAPI i s

makeUserRequest :: Settings -> Pool P.Connection -> String -> TQueue String -> UserId -> QueueRequest
makeUserRequest s p schema out u = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleUserResponse s p schema out
    where uri = userAPI u s

makeTagPageRequest :: Settings -> Pool P.Connection -> String -> TQueue String -> PageNo -> QueueRequest
makeTagPageRequest s po schema out p = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleTagPageResponse s po schema out
    where uri = tagsAPI p s
