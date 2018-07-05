module Main where

import Control.Concurrent.Async
import Control.Exception (AsyncException)
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent
import Control.Concurrent.STM
import DerpAPI
import Datas
import Data.Pool
import Database.Pool
import Database.Loader
import Config
import APIGetter
import Processing
import DataHelpers
import RequestQueues
import Helpers

main :: IO ()
main = do
    -- START SETUP --
    -- Get config file data
    settings <- getSettings
    creds    <- getDatabaseCreds

    -- Start the connection pool
    resource <- defaultResources
    pool     <- getPool resource (db_database creds)

    -- Create new rate-limit, output, and results
    rl      <- atomically $ newTBQueue $ (*) 2 $ num_request_threads settings
    out     <- atomically $ newTQueue
    results <- atomically $ newTVar []

    -- Create necessary data
    let imageList    = map (makeImageRequest settings out) [(load_image_start settings)..(load_image_end settings)]
        userList     = map (makeUserRequest settings out)  [(load_user_start settings)..(load_user_end settings)]
        combinedList = zipLists imageList userList -- [image, user, image, user, image, user, image, etc.]
        threadCount  = num_request_threads settings

    -- START TASKS --

    -- Flags for when output queuing is complete
    oComplete <- atomically $ newTMVar 0

    -- Start all necessary loader/output/rate-limiting threads
    dripSemThread <- forkIO $ dripSem rl (requests_per_second settings)
    outThread     <- forkIO $ processTQueue () out handleOut $ Just oComplete

    -- Being program
    writeOut out "Running..."
    complete      <- doRequestsMulti combinedList results (Just rl) threadCount
    
    -- END START TASKS --

    let threads = [outThread, dripSemThread]

    -- Create async waiter
    waiter <- async $ atomically $ do
        ipOut  <- takeTMVar oComplete
        -- If all are empty, then the run is complete
        unless (all (==True) [(ipOut==0)]) retry
        return ()

    catch
        (wait waiter) -- Wait on all queues to be empty
        (\e -> do
            putStrLn $ show (e :: AsyncException)
            mapM_ killThread threads
            putStrLn "Killed threads"
            return ()
        )

    -- Kill all the threads
    mapM_ killThread threads
    putStrLn "Completed."

-- Handling
handleOut :: () -> String -> IO ()
handleOut _ s = putStrLn s

handleImageResponse :: Settings -> TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleImageResponse sett out rq req resp = do
    writeOut out $ "Handling image. Got " ++ show status
    if status >= 200 && status < 300 then do
        case image of
            Image i -> do
                when (load_full_images sett) $ do
                    comments <- getImageComments' (image_id i) (image_comment_count i) sett out (requestRateLimiter rq)
                    writeOut out $ show comments
                writeOut out $ show image
            DuplicateImage i -> writeOut out $ show image
            DeletedImage i -> writeOut out $ show image
            NullImage -> writeOut out $ show image
    else do
        handleBadResponse out rq req resp
    where image = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

handleUserResponse :: Settings -> TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleUserResponse sett out rq req resp = do
    writeOut out $ "Handling user. Got " ++ show status
    if status >= 200 && status < 300 then do
        case user of
            User{} -> do
                when (load_full_users sett) $ do
                    faves <- getUserFavorites' (user_name user) sett out (requestRateLimiter rq)
                    writeOut out $ show faves
                writeOut out $ show user
            NullUser -> writeOut out $ show user
            _  -> writeOut out "Uh oh"
    else do
        handleBadResponse out rq req resp
    where user = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

handleTagPageResponse :: Settings -> TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleTagPageResponse sett out rq req resp = do
    writeOut out $ "Handling tag page. Got " ++ (show $ queueResponseStatus resp)

makeImageRequest :: Settings -> TQueue String -> ImageId -> QueueRequest
makeImageRequest s out i = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleImageResponse s out
    where uri = imageAPI i s

makeUserRequest :: Settings -> TQueue String -> UserId -> QueueRequest
makeUserRequest s out u = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleUserResponse s out
    where uri = userAPI u s

makeTagPageRequest :: Settings -> TQueue String -> PageNo -> QueueRequest
makeTagPageRequest s out p = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleTagPageResponse s out
    where uri = tagsAPI p s
