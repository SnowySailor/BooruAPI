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

main :: IO ()
main = do
    -- Setup
    resource <- defaultResources
    settings <- getSettings
    creds    <- getDatabaseCreds
    pool     <- getPool resource (db_database creds)
    sched    <- atomically $ do
        iq  <- newTBQueue 10
        uq  <- newTBQueue 10
        irq <- newTQueue
        urq <- newTQueue
        out <- newTQueue
        return $ Scheduler {
            schedImageQueue      = iq,
            schedUserQueue       = uq,
            schedImageRetryQueue = irq,
            schedUserRetryQueue  = urq,
            schedOut             = out
        }
    let appSettings = AppSettings {
            app_settings = settings,
            app_db_creds = creds,
            app_db_pool  = pool
        }

    -- Start tasks

    let imageList = [(load_image_start settings)..(load_image_end settings)]
        userList  = [(load_user_start settings)..(load_user_end settings)]
    populateImageQueueThread <- forkIO $ addToQueue (schedImageQueue sched) imageList
    populateUserQueueThread  <- forkIO $ addToQueue (schedUserQueue sched) userList
    imageThread              <- forkIO $ processTBQueue sched schedImageQueue $ processImage appSettings
    userThread               <- forkIO $ processTBQueue sched schedUserQueue  $ processUser appSettings
    imageRetryThread         <- forkIO $ processTQueue sched schedImageRetryQueue $ processImageRetry appSettings
    userRetryThread          <- forkIO $ processTQueue sched schedUserRetryQueue  $ processUserRetry appSettings
    outThread                <- forkIO $ processTQueue sched schedOut processOut
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTBQueue $ schedImageQueue sched
        empty2 <- isEmptyTBQueue $ schedUserQueue sched
        empty3 <- isEmptyTQueue  $ schedImageRetryQueue sched
        empty4 <- isEmptyTQueue  $ schedUserRetryQueue sched
        empty5 <- isEmptyTQueue  $ schedOut sched
        unless (all (==True) [empty1, empty2, empty3, empty4, empty5]) retry
        return ()
    putStrLn "Running..."
    catch
        (wait waiter) -- Wait on all queues to be empty
        (\e -> do
            putStrLn $ show (e :: AsyncException)
            mapM_ killThread [populateUserQueueThread, populateImageQueueThread, imageThread, userThread, imageRetryThread,
                              userRetryThread, outThread]
            putStrLn "Killed threads"
            return ()
        )
    -- Kill all the threads
    mapM_ killThread [populateUserQueueThread, populateImageQueueThread, imageThread, userThread, imageRetryThread, userRetryThread, outThread]
    putStrLn "Completed."

-- Preparing
addToQueue :: (Traversable t) => TBQueue a -> t a -> IO ()
addToQueue q l = forM_ l $ \x -> atomically $ writeTBQueue q x

-- Processing
processTBQueue :: a -> (a -> TBQueue b) -> (a -> b -> IO c) -> IO c
processTBQueue sched q f = forever $ do
    toProcess <- atomically $ readTBQueue (q sched)
    f sched toProcess

processTQueue :: a -> (a -> TQueue b) -> (a -> b -> IO c) -> IO c
processTQueue sched q f = forever $ do
    toProcess <- atomically $ readTQueue (q sched)
    f sched toProcess

processImage :: AppSettings -> Scheduler -> ImageId -> IO ()
processImage sett sched i = do
    result <- case load_full_images $ app_settings sett of
        True  -> handleImageFull sett sched i
        False -> handleImage sett sched i
    putStrLn $ show result

processUser :: AppSettings -> Scheduler -> UserId -> IO ()
processUser sett sched u = do
    result <- case load_full_users $ app_settings sett of
        True  -> handleUserFull sett sched u
        False -> handleUser sett sched u
    putStrLn $ show result

processImageRetry :: AppSettings -> Scheduler -> Request -> IO ()
processImageRetry _ _ _ = return ()

processUserRetry :: AppSettings -> Scheduler -> Request -> IO ()
processUserRetry _ _ _ = return ()

processOut :: Scheduler -> String -> IO ()
processOut _ s = putStrLn s

-- Doing
handleUserFull :: AppSettings -> Scheduler -> UserId -> IO (Int64, Int64, Int64, Int64)
handleUserFull sett sched u = do
    (user, status) <- getUserFull u (app_settings sett)
    case status of
        200 -> withResource (app_db_pool sett) $ \conn -> loadUserFull user conn (db_schema $ app_db_creds sett)
        _   -> do
            atomically $ writeTQueue (schedUserRetryQueue sched) $ Request u 0 [status]
            return (0,0,0,0)

handleUser :: AppSettings -> Scheduler -> UserId -> IO (Int64, Int64, Int64, Int64)
handleUser sett sched u = do
    (user, status) <- getUser u (app_settings sett)
    case status of
        200 -> withResource (app_db_pool sett) $ \conn -> loadUser user conn (db_schema $ app_db_creds sett)
        _   -> do
            atomically $ writeTQueue (schedUserRetryQueue sched) $ Request u 0 [status]
            return (0,0,0,0)

handleImage :: AppSettings -> Scheduler -> ImageId -> IO (Int64, Int64, Int64)
handleImage sett sched i = do
    (image, status) <- getImage i (app_settings sett)
    case status of
        200 -> withResource (app_db_pool sett) $ \conn -> loadImage image conn (db_schema $ app_db_creds sett)
        _   -> do
            atomically $ writeTQueue (schedImageRetryQueue sched) $ Request i 0 [status]
            return (0,0,0)

handleImageFull :: AppSettings -> Scheduler -> ImageId -> IO (Int64, Int64, Int64)
handleImageFull sett sched i = do
    (image, status) <- getImageFull i (app_settings sett)
    case status of
        200 -> withResource (app_db_pool sett) $ \conn -> loadImageFull image conn (db_schema $ app_db_creds sett)
        _   -> do
            atomically $ writeTQueue (schedImageRetryQueue sched) $ Request i 0 [status]
            return (0,0,0)
