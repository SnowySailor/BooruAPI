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
import GHC.Int
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client as C
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import APIGetter
import Processing
import DataManipulation

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
    outThread                  <- forkIO $ processTQueue sched schedOut handleOut

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

writeOut :: (Show a) => AppContext -> a -> IO ()
writeOut ctx o = atomically $ writeTQueue (schedOut $ app_sched ctx) $ show o

-- Handling
handleOut :: Scheduler -> String -> IO ()
handleOut _ s = putStrLn s

handleImageResponse :: AppContext -> ByteString -> Int -> IO ()
handleImageResponse ctx bs status = do
    addToTQueue ("Handling image. Got " ++ show status) (schedOut $ app_sched ctx)
    when (status >= 200 && status < 300) $ do
        case image of
            Image i          -> writeOut ctx image
            DuplicateImage i -> writeOut ctx image
            DeletedImage i   -> writeOut ctx image
            NullImage        -> writeOut ctx image
        where image = decodeNoMaybe bs
    --when (status >= 300 && status < 400) $ putStrLn "Redirected"
    --when (status >= 400 && status < 500) $ putStrLn "Client error"
    --when (status >= 500 && status < 600) $ putStrLn "Server error"

handleUserResponse :: AppContext -> ByteString -> Int -> IO ()
handleUserResponse ctx bs status = do
    addToTQueue ("Handling user. Got " ++ show status) (schedOut $ app_sched ctx)
    when (status >= 200 && status < 300) $ do
        case user of
            User{}   -> writeOut ctx user
            NullUser -> writeOut ctx user
            _        -> writeOut ctx "Uh oh"
        where user = decodeNoMaybe bs
    -- when (status >= 300 && status < 400) $ putStrLn "Redirected"
    -- when (status >= 400 && status < 500) $ putStrLn "Client error"
    -- when (status >= 500 && status < 600) $ putStrLn "Server error"

handleTagPageResponse :: AppContext -> ByteString -> Int -> IO ()
handleTagPageResponse ctx bs status = do
    addToTQueue ("Handling tag page. Got " ++ show status) (schedOut $ app_sched ctx)

handleCommentPageResponse :: AppContext -> ByteString -> Int -> IO ()
handleCommentPageResponse ctx bs status = do
    addToTQueue ("Handling comment page. Got " ++ show status) (schedOut $ app_sched ctx)

handleSearchPageResponse :: AppContext -> ByteString -> Int -> IO ()
handleSearchPageResponse ctx bs status = do
    addToTQueue ("Handling search page. Got " ++ show status) (schedOut $ app_sched ctx)


makeImageRequest :: Settings -> ImageId -> AppRequest
makeImageRequest s i = AppRequest uri Nothing GET handleImageResponse
    where uri = imageAPI i s

makeUserRequest :: Settings -> UserId -> AppRequest
makeUserRequest s u = AppRequest uri Nothing GET handleUserResponse
    where uri = userAPI u s

makeTagPageRequest :: Settings -> PageNo -> AppRequest
makeTagPageRequest s p = AppRequest uri Nothing GET handleTagPageResponse
    where uri = tagsAPI p s

makeCommentPageRequest :: Settings -> ImageId -> PageNo -> AppRequest
makeCommentPageRequest s i p = AppRequest uri Nothing GET handleCommentPageResponse
    where uri = commentsAPI i p s

makeSearchPageRequest :: Settings -> String -> PageNo -> AppRequest
makeSearchPageRequest s q p = AppRequest uri Nothing GET handleSearchPageResponse
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
