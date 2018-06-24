module Main where

import Control.Concurrent.Async
import Control.Exception (AsyncException)
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue
import DerpAPI
import Datas
import Data.Pool
import Database.Pool
import Database.Loader
import Config

main :: IO ()
main = do
    -- Setup
    resource <- defaultResources
    pool     <- getPool resource "database"
    settings <- getSettings
    creds    <- getDatabaseCreds
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
    let schema = db_schema creds

    -- Start tasks

    let imageList = [(load_image_start settings)..(load_image_end settings)]
        userList  = [(load_user_start settings)..(load_user_end settings)]
    populateImageQueueThread <- forkIO $ addToQueue (schedImageQueue sched) imageList
    populateUserQueueThread  <- forkIO $ addToQueue (schedUserQueue sched) userList
    imageThread              <- forkIO $ processTBQueue sched schedImageQueue processImage
    userThread               <- forkIO $ processTBQueue sched schedUserQueue processUser
    imageRetryThread         <- forkIO $ processTQueue sched schedImageRetryQueue processImageRetry
    userRetryThread          <- forkIO $ processTQueue sched schedUserRetryQueue processUserRetry
    outThread                <- forkIO $ processTQueue sched schedOut processOut
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTBQueue $ schedImageQueue sched
        empty2 <- isEmptyTBQueue $ schedUserQueue sched
        empty3 <- isEmptyTQueue  $ schedImageRetryQueue sched
        empty4 <- isEmptyTQueue  $ schedUserRetryQueue sched
        unless (all (==True) [empty1, empty2, empty3, empty4]) retry
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

addToQueue :: (Traversable t) => TBQueue a -> t a -> IO ()
addToQueue q l = forM_ l $ \x -> atomically $ writeTBQueue q x

processTBQueue :: a -> (a -> TBQueue b) -> (a -> b -> IO c) -> IO c
processTBQueue sched q f = forever $ do
    toProcess <- atomically $ readTBQueue (q sched)
    f sched toProcess

processTQueue :: a -> (a -> TQueue b) -> (a -> b -> IO c) -> IO c
processTQueue sched q f = forever $ do
    toProcess <- atomically $ readTQueue (q sched)
    f sched toProcess

processImage :: Scheduler -> ImageId -> IO ()
processImage _ _ = return ()

processUser :: Scheduler -> UserId -> IO ()
processUser _ _ = return ()

processImageRetry :: Scheduler -> Request -> IO ()
processImageRetry _ _ = return ()

processUserRetry :: Scheduler -> Request -> IO ()
processUserRetry _ _ = return ()

processOut :: Scheduler -> String -> IO ()
processOut _ s = putStrLn s

data Request = Request {
    requestId         :: Int,
    requestTries      :: Int,
    requestRespCodes  :: [Int]
} deriving (Show)

data Scheduler = Scheduler {
    schedImageQueue      :: TBQueue Int,
    schedUserQueue       :: TBQueue Int,
    schedImageRetryQueue :: TQueue Request,
    schedUserRetryQueue  :: TQueue Request,
    schedOut             :: TQueue String
}
