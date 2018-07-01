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
import Data.ByteString.Lazy (ByteString)
import APIGetter
import Processing
import DataHelpers

main :: IO ()
main = do
    -- START SETUP --
    -- Get config file data
    settings <- getSettings
    creds    <- getDatabaseCreds

    -- Start the connection pool
    resource <- defaultResources
    pool     <- getPool resource (db_database creds)

    -- Create a scheduler and app context
    sched    <- atomically $ do
        rq   <- newTBQueue $ (*) 2 $ num_request_threads settings
        rl   <- newTBQueue $ (*) 2 $ num_request_threads settings
        retq <- newTQueue
        out  <- newTQueue
        return Scheduler {
            schedRequestQueue = rq,
            schedRetryQueue   = retq,
            schedRateLimiter  = rl,
            schedReqPerSec    = requests_per_second settings,
            schedOut          = out
        }
    let ctx = AppContext {
        app_sett      = settings,
        app_sched     = sched,
        app_db_creds  = creds,
        app_db_pool   = pool
    }

    -- Create necessary data
    let imageList = map (makeImageRequest settings) [(load_image_start settings)..(load_image_end settings)]
        userList  = map (makeUserRequest settings)  [(load_user_start settings)..(load_user_end settings)]
        threadNum = num_request_threads settings

    -- START TASKS --

    -- Flags for when image/user queuing is complete
    iComplete <- atomically $ newTMVar ()
    uComplete <- atomically $ newTMVar ()
    -- Start all necessary threads
    populateImageRequestThread <- forkIO $ addTraverseToTBQueue imageList (schedRequestQueue sched) iComplete
    populateUserRequestThread  <- forkIO $ addTraverseToTBQueue userList (schedRequestQueue sched) uComplete
    dripSemThread              <- forkIO $ dripSem sched
    requestThreads             <- replicateM threadNum $ forkIO $ processTBQueue ctx (schedRequestQueue . app_sched) processRequest
    retryThreads               <- replicateM threadNum $ forkIO $ processTQueue ctx (schedRetryQueue . app_sched) processRequest
    outThread                  <- forkIO $ processTQueue ctx (schedOut . app_sched) handleOut

    -- END START TASKS --

    let threads = [populateImageRequestThread, populateUserRequestThread, outThread,
                   dripSemThread] ++ requestThreads ++ retryThreads

    -- Create async waiter
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTBQueue $ schedRequestQueue sched
        empty2 <- isEmptyTQueue  $ schedRetryQueue sched
        empty3 <- isEmptyTQueue  $ schedOut sched
        empty4 <- isEmptyTMVar   $ iComplete
        empty5 <- isEmptyTMVar   $ uComplete
        -- If all are empty, then the run is complete
        unless (all (==True) [empty1, empty2, empty3, empty4, empty5]) retry
        return ()

    putStrLn "Running..."
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
handleOut :: AppContext -> String -> Maybe (TQueue String) -> IO ()
handleOut _ s _ = putStrLn s

handleImageResponse :: AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleImageResponse ctx r _ bs status = do
    addToTQueue ("Handling image. Got " ++ show status) (schedOut $ app_sched ctx)
    if status >= 200 && status < 300 then do
        case image of
            Image i -> do
                when (load_full_images $ app_sett ctx) $ do
                    comments <- getImageComments (image_id i) (image_comment_count i) ctx
                    writeOut ctx comments
                writeOut ctx image
            DuplicateImage i -> writeOut ctx image
            DeletedImage i -> writeOut ctx image
            NullImage -> writeOut ctx image
    else do
        handleBadResponse ctx r status
    where image = decodeNoMaybe bs

handleUserResponse :: AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleUserResponse ctx r _ bs status = do
    addToTQueue ("Handling user. Got " ++ show status) (schedOut $ app_sched ctx)
    if status >= 200 && status < 300 then do
        case user of
            User{} -> do
                when (load_full_users $ app_sett ctx) $ do
                    faves <- getUserFavoritesSimple (user_name user) $ app_sett ctx
                    writeOut ctx faves
                writeOut ctx user
            NullUser -> writeOut ctx user
            _  -> writeOut ctx "Uh oh"
    else do
        handleBadResponse ctx r status
    where user = decodeNoMaybe bs

handleTagPageResponse :: AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleTagPageResponse ctx r _ bs status = do
    addToTQueue ("Handling tag page. Got " ++ show status) (schedOut $ app_sched ctx)

handleCommentPageResponse :: AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleCommentPageResponse ctx r _ bs status = do
    addToTQueue ("Handling comment page. Got " ++ show status) (schedOut $ app_sched ctx)

handleSearchPageResponse :: AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleSearchPageResponse ctx r _ bs status = do
    addToTQueue ("Handling search page. Got " ++ show status) (schedOut $ app_sched ctx)

handleBadResponse :: AppContext -> AppRequest -> Int -> IO ()
handleBadResponse ctx r s = do
    if s >= 300 && s < 400 then
        writeOut ctx $ "Got " ++ show s ++ " at " ++ (requestUri r) ++ ". Not retrying."
    else if s >= 400 && s < 500 then
        case s of
            400 -> unless (requestTries r >= max_retries) $ do
                        writeOut ctx $ "Got 400 at " ++ (requestUri r) ++ ". Retrying."
                        retryRequest (incReqeust r s) ctx
            _   -> writeOut ctx $ "Got " ++ show s ++ " at " ++ (requestUri r) ++ ". Not retrying."
    else if s >= 500 && s < 600 then
        case s of
            500 -> unless (requestTries r >= max_retries) $ do
                        writeOut ctx $ "Got 500 at " ++ (requestUri r) ++ ". Retrying."
                        retryRequest (incReqeust r s) ctx
            _   -> writeOut ctx $ "Got " ++ show s ++ " at " ++ (requestUri r) ++ ". Not retrying."
    else
        writeOut ctx $ "Got " ++ show s ++ " at " ++ (requestUri r) ++ ". Not retrying."
    where max_retries = max_retry_count $ app_sett ctx

makeImageRequest :: Settings -> ImageId -> AppRequest
makeImageRequest s i = AppRequest uri Nothing GET 0 [] handleImageResponse
    where uri = imageAPI i s

makeUserRequest :: Settings -> UserId -> AppRequest
makeUserRequest s u = AppRequest uri Nothing GET 0 [] handleUserResponse
    where uri = userAPI u s

makeTagPageRequest :: Settings -> PageNo -> AppRequest
makeTagPageRequest s p = AppRequest uri Nothing GET 0 [] handleTagPageResponse
    where uri = tagsAPI p s

makeCommentPageRequest :: Settings -> ImageId -> PageNo -> AppRequest
makeCommentPageRequest s i p = AppRequest uri Nothing GET 0 [] handleCommentPageResponse
    where uri = commentsAPI i p s

makeSearchPageRequest :: Settings -> String -> PageNo -> AppRequest
makeSearchPageRequest s q p = AppRequest uri Nothing GET 0 [] handleSearchPageResponse
    where uri = searchAPI q p s

-- processImage :: AppContext -> Scheduler -> ImageId -> IO ()
-- processImage sett sched i = do
--     result <- case load_full_images $ app_sett sett of
--         True  -> handleImageFull sett sched i
--         False -> handleImage sett sched i
--     putStrLn $ show result

-- processUser :: AppContext -> Scheduler -> UserId -> IO ()
-- processUser sett sched u = do
--     result <- case load_full_users $ app_sett sett of
--         True  -> handleUserFull sett sched u
--         False -> handleUser sett sched u
--     putStrLn $ show result

-- processImageRetry :: AppContext -> Scheduler -> AppRequest -> IO ()
-- processImageRetry _ _ _ = return ()

-- processUserRetry :: AppContext -> Scheduler -> AppRequest -> IO ()
-- processUserRetry _ _ _ = return ()

-- -- Doing
-- handleUserFull :: AppContext -> Scheduler -> UserId -> IO (Int64, Int64, Int64, Int64)
-- handleUserFull sett sched u = do
--     (user, status) <- getUserFull u (app_sett sett)
--     case status of
--         200 -> withResource (app_db_pool sett) $ \conn -> loadUserFull user conn (db_schema $ app_db_creds sett)
--         _   -> do
--             atomically $ writeTQueue (schedUserRetryQueue sched) $ AppRequest u 0 [status]
--             return (0,0,0,0)

-- handleUser :: AppContext -> Scheduler -> UserId -> IO (Int64, Int64, Int64, Int64)
-- handleUser sett sched u = do
--     (user, status) <- getUser u (app_sett sett)
--     case status of
--         200 -> do
--             (a,b,c) <- withResource (app_db_pool sett) $ \conn -> loadUser user conn (db_schema $ app_db_creds sett)
--             return (a,b,c,0)
--         _   -> do
--             putStrLn $ "Got " ++ show status ++ " from user. Retrying."
--             atomically $ writeTQueue (schedUserRetryQueue sched) $ AppRequest u 0 [status]
--             return (0,0,0,0)

-- handleImage :: AppContext -> Scheduler -> ImageId -> IO (Int64, Int64, Int64)
-- handleImage sett sched i = do
--     (image, status) <- getImage i (app_sett sett)
--     case status of
--         200 -> do
--             (a,b) <- withResource (app_db_pool sett) $ \conn -> loadImage image conn (db_schema $ app_db_creds sett)
--             return (a,b,0)
--         _   -> do
--             putStrLn $ "Got " ++ show status ++ " from image. Retrying."
--             atomically $ writeTQueue (schedImageRetryQueue sched) $ AppRequest i 0 [status]
--             return (0,0,0)

-- handleImageFull :: AppContext -> Scheduler -> ImageId -> IO (Int64, Int64, Int64)
-- handleImageFull sett sched i = do
--     (image, status) <- getImageFull i (app_sett sett)
--     case status of
--         200 -> withResource (app_db_pool sett) $ \conn -> loadImageFull image conn (db_schema $ app_db_creds sett)
--         _   -> do
--             atomically $ writeTQueue (schedImageRetryQueue sched) $ AppRequest i 0 [status]
--             return (0,0,0)
