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

main :: IO ()
main = do
    -- Setup
    resource <- defaultResources
    settings <- getSettings
    creds    <- getDatabaseCreds
    pool     <- getPool resource (db_database creds)
    sched    <- atomically $ do
        rq   <- newTBQueue $ (*) 2 $ num_request_threads settings
        rl   <- newTBQueue $ (*) 2 $ num_request_threads settings
        retq <- newTQueue
        out  <- newTQueue
        return $ Scheduler {
            schedRequestQueue = rq,
            schedRetryQueue   = retq,
            schedRateLimiter  = rl,
            schedReqPerSec    = requests_per_second settings,
            schedOut          = out
        }
    let appSettings = AppSettings {
            app_settings = settings,
            app_db_creds = creds,
            app_db_pool  = pool
        }

    -- Start tasks

    let imageList = map (makeImageRequest settings) [(load_image_start settings)..(load_image_end settings)]
        userList  = map (makeUserRequest settings)  [(load_user_start settings)..(load_user_end settings)]
    iComplete <- atomically $ newTMVar ()
    uComplete <- atomically $ newTMVar ()

    populateImageRequestThread <- forkIO $ addTraverseToTBQueue imageList (schedRequestQueue sched) iComplete
    populateUserRequestThread  <- forkIO $ addTraverseToTBQueue userList (schedRequestQueue sched) uComplete
    dripSemThread              <- forkIO $ dripSem sched
    requestThread              <- forkIO $ processTBQueue sched schedRequestQueue processRequest
    retryThread                <- forkIO $ processTQueue sched schedRetryQueue processRequest
    outThread                  <- forkIO $ processTQueue sched schedOut handleOut
    let threads = [populateImageRequestThread, populateUserRequestThread, requestThread, retryThread, outThread,
                   dripSemThread]
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTBQueue $ schedRequestQueue sched
        empty2 <- isEmptyTQueue  $ schedRetryQueue sched
        empty3 <- isEmptyTQueue  $ schedOut sched
        empty4 <- isEmptyTMVar   $ iComplete
        empty5 <- isEmptyTMVar   $ uComplete
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

-- Trampolining
    -- tail call recursion
    -- create a queue for its own thread

dripSem :: Scheduler -> IO ()
dripSem sched = forever $ do
    atomically $ writeTBQueue (schedRateLimiter sched) ()
    threadDelay $ (*) 1000000 $ floor $ 1/(schedReqPerSec sched)

rateLimit :: Scheduler -> IO ()
rateLimit sched = atomically $ readTBQueue $ schedRateLimiter sched

processRequest :: Scheduler -> AppRequest -> IO ()
processRequest sched req = do
    case requestMethod req of
        GET -> do
            req     <- parseRequest $ requestUri req
            manager <- newManager tlsManagerSettings
            rateLimit sched
            resp    <- C.httpLbs req manager
            callback (responseBody resp) (statusCode $ responseStatus resp)
            where callback = requestCallback req
        _ -> undefined

-- Preparing
addTraverseToTBQueue :: (Traversable t) => t a -> TBQueue a -> TMVar b -> IO b
addTraverseToTBQueue l q c = do
    forM_ l $ \x -> atomically $ writeTBQueue q x -- Perform the write task
    atomically $ takeTMVar c -- Notify that we've completed the task

addToTBQueue :: a -> TBQueue a -> IO ()
addToTBQueue x q = atomically $ writeTBQueue q x

-- Processing
processTBQueue :: a -> (a -> TBQueue b) -> (a -> b -> IO c) -> IO c
processTBQueue sched q f = forever $ do
    toProcess <- atomically $ readTBQueue (q sched) -- Get the next value from the queue
    f sched toProcess -- Process the value and return the result

processTQueue :: a -> (a -> TQueue b) -> (a -> b -> IO c) -> IO c
processTQueue sched q f = forever $ do
    toProcess <- atomically $ readTQueue (q sched) -- Get the next value from the queue
    f sched toProcess -- Process the value and return the result


-- Handling
handleOut :: Scheduler -> String -> IO ()
handleOut _ s = putStrLn s

handleImageResponse :: ByteString -> Int -> IO ()
handleImageResponse bs status = do
    putStrLn $ "Handling image. Got " ++ show status

handleUserResponse :: ByteString -> Int -> IO ()
handleUserResponse bs status = do
    putStrLn $ "Handling user. Got " ++ show status

makeImageRequest :: Settings -> ImageId -> AppRequest
makeImageRequest s i = AppRequest uri Nothing GET handleImageResponse
    where uri = imageAPI i s

makeUserRequest :: Settings -> UserId -> AppRequest
makeUserRequest s u = AppRequest uri Nothing GET handleUserResponse
    where uri = userAPI u s

-- processImage :: AppSettings -> Scheduler -> ImageId -> IO ()
-- processImage sett sched i = do
--     result <- case load_full_images $ app_settings sett of
--         True  -> handleImageFull sett sched i
--         False -> handleImage sett sched i
--     putStrLn $ show result

-- processUser :: AppSettings -> Scheduler -> UserId -> IO ()
-- processUser sett sched u = do
--     result <- case load_full_users $ app_settings sett of
--         True  -> handleUserFull sett sched u
--         False -> handleUser sett sched u
--     putStrLn $ show result

-- processImageRetry :: AppSettings -> Scheduler -> AppRequest -> IO ()
-- processImageRetry _ _ _ = return ()

-- processUserRetry :: AppSettings -> Scheduler -> AppRequest -> IO ()
-- processUserRetry _ _ _ = return ()

-- -- Doing
-- handleUserFull :: AppSettings -> Scheduler -> UserId -> IO (Int64, Int64, Int64, Int64)
-- handleUserFull sett sched u = do
--     (user, status) <- getUserFull u (app_settings sett)
--     case status of
--         200 -> withResource (app_db_pool sett) $ \conn -> loadUserFull user conn (db_schema $ app_db_creds sett)
--         _   -> do
--             atomically $ writeTQueue (schedUserRetryQueue sched) $ AppRequest u 0 [status]
--             return (0,0,0,0)

-- handleUser :: AppSettings -> Scheduler -> UserId -> IO (Int64, Int64, Int64, Int64)
-- handleUser sett sched u = do
--     (user, status) <- getUser u (app_settings sett)
--     case status of
--         200 -> do
--             (a,b,c) <- withResource (app_db_pool sett) $ \conn -> loadUser user conn (db_schema $ app_db_creds sett)
--             return (a,b,c,0)
--         _   -> do
--             putStrLn $ "Got " ++ show status ++ " from user. Retrying."
--             atomically $ writeTQueue (schedUserRetryQueue sched) $ AppRequest u 0 [status]
--             return (0,0,0,0)

-- handleImage :: AppSettings -> Scheduler -> ImageId -> IO (Int64, Int64, Int64)
-- handleImage sett sched i = do
--     (image, status) <- getImage i (app_settings sett)
--     case status of
--         200 -> do
--             (a,b) <- withResource (app_db_pool sett) $ \conn -> loadImage image conn (db_schema $ app_db_creds sett)
--             return (a,b,0)
--         _   -> do
--             putStrLn $ "Got " ++ show status ++ " from image. Retrying."
--             atomically $ writeTQueue (schedImageRetryQueue sched) $ AppRequest i 0 [status]
--             return (0,0,0)

-- handleImageFull :: AppSettings -> Scheduler -> ImageId -> IO (Int64, Int64, Int64)
-- handleImageFull sett sched i = do
--     (image, status) <- getImageFull i (app_settings sett)
--     case status of
--         200 -> withResource (app_db_pool sett) $ \conn -> loadImageFull image conn (db_schema $ app_db_creds sett)
--         _   -> do
--             atomically $ writeTQueue (schedImageRetryQueue sched) $ AppRequest i 0 [status]
--             return (0,0,0)

data HTTPMethod = GET | POST

data AppRequest = AppRequest {
    requestUri      :: String,
    requestBody     :: Maybe ByteString,
    requestMethod   :: HTTPMethod,
    requestCallback :: ByteString -> Int -> IO ()
}

data Scheduler = Scheduler {
    schedRequestQueue :: TBQueue AppRequest,
    schedRetryQueue   :: TQueue AppRequest,
    schedRateLimiter  :: TBQueue (),
    schedReqPerSec    :: Double,
    schedOut          :: TQueue String
}