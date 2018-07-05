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


    -- Create new request, retry, rate-limit, and results variables
    rq      <- atomically $ newTBQueue $ (*) 2 $ num_request_threads settings
    retq    <- atomically $ newTQueue
    rl      <- atomically $ newTBQueue $ (*) 2 $ num_request_threads settings
    out     <- atomically $ newTQueue
    results <- atomically $ newTMVar []

    -- Create necessary data
    let imageList    = map (makeImageRequest settings out) [(load_image_start settings)..(load_image_end settings)]
        userList     = map (makeUserRequest settings out)  [(load_user_start settings)..(load_user_end settings)]
        combinedList = zipLists imageList userList -- [image, user, image, user, image, user, image, etc.]
        threadCount  = num_request_threads settings

    -- START TASKS --

    -- Flags for when image/user/output queuing is complete
    iComplete <- atomically $ newTMVar ()
    uComplete <- atomically $ newTMVar ()
    oComplete <- atomically $ newTMVar 0

    -- Start all necessary loader/output/rate-limiting threads
    dripSemThread <- forkIO $ dripSem rl (requests_per_second settings)
    outThread     <- forkIO $ processTQueue 0 out handleOut $ Just oComplete
    complete      <- doRequestsMulti combinedList results (Just rl) threadCount
    
    -- END START TASKS --

    let threads = [outThread, dripSemThread]

    -- Create async waiter
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTMVar iComplete
        empty2 <- isEmptyTMVar uComplete
        ipOut  <- takeTMVar    oComplete
        -- If all are empty, then the run is complete
        unless (all (==True) [empty1, empty2, (ipOut==0)]) retry
        return ()

    writeOut out "Running..."
    catch
        (wait waiter) -- Wait on all queues to be empty
        (\e -> do
            putStrLn $ show (e :: AsyncException)
            mapM_ killThread threads
            putStrLn "Killed threads"
            return ()
        )
    threadDelay 10000000
    -- Kill all the threads
    mapM_ killThread threads
    putStrLn "Completed."

-- Handling
handleOut :: Int -> String -> IO ()
handleOut _ s = putStrLn s

handleImageResponse :: TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleImageResponse out rq req resp = do
    writeOut out $ "Handling image. Got " ++ show status
    if status >= 200 && status < 300 then do
        case image of
            Image i -> do
                -- when (load_full_images $ app_sett ctx) $ do
                --     comments <- getImageComments (image_id i) (image_comment_count i) ctx
                --     writeOut out comments
                writeOut out $ show image
            DuplicateImage i -> writeOut out $ show image
            DeletedImage i -> writeOut out $ show image
            NullImage -> writeOut out $ show image
    else do
        handleBadResponse out rq req resp
    where image = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

handleUserResponse :: TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleUserResponse out rq req resp = do
    writeOut out $ "Handling user. Got " ++ show status
    if status >= 200 && status < 300 then do
        case user of
            User{} -> do
                -- when (load_full_users $ app_sett ctx) $ do
                --     faves <- getUserFavorites (user_name user) ctx
                --     writeOut ctx faves
                writeOut out $ show user
            NullUser -> writeOut out $ show user
            _  -> writeOut out "Uh oh"
    else do
        handleBadResponse out rq req resp
    where user = decodeNoMaybe $ queueResponseBody resp
          status = queueResponseStatus resp

handleTagPageResponse :: TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleTagPageResponse out rq req resp = do
    writeOut out $ "Handling tag page. Got " ++ (show $ queueResponseStatus resp)

handleBadResponse :: TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleBadResponse out rq req resp = do
    if status >= 300 && status < 400 then
        writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    else if status >= 400 && status < 500 then
        case status of
            400 -> unless (requestTries req >= max_retries) $ do
                        writeOut out $ "Got 400 at " ++ (requestUri req) ++ ". Retrying."
                        retryRequest req resp rq
            _   -> writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    else if status >= 500 && status < 600 then
        case status of
            500 -> unless (requestTries req >= max_retries) $ do
                        writeOut out $ "Got 500 at " ++ (requestUri req) ++ ". Retrying."
                        retryRequest req resp rq
            _   -> writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    else
        writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    where max_retries = requestTriesMax req
          status = queueResponseStatus resp

makeImageRequest :: Settings -> TQueue String -> ImageId -> QueueRequest
makeImageRequest s out i = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleImageResponse out
    where uri = imageAPI i s

makeUserRequest :: Settings -> TQueue String -> UserId -> QueueRequest
makeUserRequest s out u = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleUserResponse out
    where uri = userAPI u s

makeTagPageRequest :: Settings -> TQueue String -> PageNo -> QueueRequest
makeTagPageRequest s out p = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleTagPageResponse out
    where uri = tagsAPI p s
