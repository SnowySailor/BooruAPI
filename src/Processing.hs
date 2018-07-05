module Processing where

import Datas
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Network.HTTP.Client as C
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import DataHelpers

-- Trampolining
    -- tail call recursion
    -- create a queue for its own thread

-- Rate limiting
dripSem :: (RealFrac a) => RateLimiter -> a -> IO ()
dripSem rl rate = forever $ do
     -- Write that we have a spot open
    atomically $ writeTBQueue rl ()
     -- Wait a certin amount of time before opening a new spot
    threadDelay $ (*) 1000000 $ floor $ 1/rate

writeOut :: (Show a) => TQueue a -> a -> IO ()
writeOut q e = atomically $ writeTQueue q e

-- Adding to queues

-- Meant to be used asynchronously. It will notify the caller when complete by taking the TMVar passed
addTraverseToTBQueue :: (Traversable t) => t a -> TBQueue a -> TMVar b -> IO b
addTraverseToTBQueue l q c = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTBQueue q x
    -- Notify that we've completed the task
    atomically $ takeTMVar c

-- Meant to be used synchronously and it won't notify the caller when it is complete
addTraverseToTBQueueSync :: (Traversable t) => t a -> TBQueue a -> IO ()
addTraverseToTBQueueSync l q = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTBQueue q x

-- Meant to be used asynchronously. It will notify the caller when complete by taking the TMVar passed
addTraverseToTQueue :: (Traversable t) => t a -> TQueue a -> TMVar b -> IO b
addTraverseToTQueue l q c = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTQueue q x
    -- Notify that we've completed the task
    atomically $ takeTMVar c

-- Meant to be used synchronously and it won't notify the caller when it is complete
addTraverseToTQueueSync :: (Traversable t) => t a -> TQueue a -> IO ()
addTraverseToTQueueSync l q = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTQueue q x

addToTBQueue :: a -> TBQueue a -> IO ()
addToTBQueue x q = atomically $ writeTBQueue q x

addToTQueue :: a -> TQueue a -> IO ()
addToTQueue x q = atomically $ writeTQueue q x

writeTQueueM :: Maybe (TQueue a) -> a -> IO ()
writeTQueueM mQ e =
    case mQ of
        Just q -> atomically $ writeTQueue q e
        Nothing -> return ()

writeTBQueueM :: Maybe (TBQueue a) -> a -> IO ()
writeTBQueueM mQ e =
    case mQ of
        Just q -> atomically $ writeTBQueue q e
        Nothing -> return ()

retryRequest :: QueueRequest -> QueueResponse -> RequestQueues -> IO ()
retryRequest req resp rq = unless (requestTries req > requestTriesMax req) $
    atomically $
        writeTQueue (requestQueuesRetry rq) nr
        where nr = incReqeust req $ queueResponseStatus resp

incReqeust :: QueueRequest -> Int -> QueueRequest
incReqeust (QueueRequest u b m t c tm cb) s = QueueRequest u b m (t+1) (s:c) tm cb
