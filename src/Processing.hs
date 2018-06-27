module Processing where

import Datas
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Network.HTTP.Client as C
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import DataManipulation

-- Trampolining
    -- tail call recursion
    -- create a queue for its own thread

dripSem :: Scheduler -> IO ()
dripSem sched = forever $ do
     -- Write that we have a spot open
    atomically $ writeTBQueue (schedRateLimiter sched) ()
     -- Wait a certin amount of time before opening a new spot
    threadDelay $ (*) 1000000 $ floor $ 1/(schedReqPerSec sched)

rateLimit :: Scheduler -> IO ()
rateLimit sched = atomically $ readTBQueue $ schedRateLimiter sched

-- Preparing
addTraverseToTBQueue :: (Traversable t) => t a -> TBQueue a -> TMVar b -> IO b
addTraverseToTBQueue l q c = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTBQueue q x
    -- Notify that we've completed the task
    atomically $ takeTMVar c

addTraverseToTQueueSync :: (Traversable t) => t a -> TQueue a -> IO ()
addTraverseToTQueueSync l q = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTQueue q x

addToTBQueue :: a -> TBQueue a -> IO ()
addToTBQueue x q = atomically $ writeTBQueue q x

addToTQueue :: a -> TQueue a -> IO ()
addToTQueue x q = atomically $ writeTQueue q x

-- Processing
processTBQueue :: a -> (a -> TBQueue b) -> (a -> b -> IO c) -> IO c
processTBQueue sched q f = forever $ do
     -- Get the next value from the queue
    toProcess <- atomically $ readTBQueue (q sched)
     -- Process the value and return the result
    f sched toProcess

processTQueue :: a -> (a -> TQueue b) -> (a -> b -> IO c) -> IO c
processTQueue sched q f = forever $ do
     -- Get the next value from the queue
    toProcess <- atomically $ readTQueue (q sched)
     -- Process the value and return the result
    f sched toProcess

processRequest :: AppContext -> AppRequest -> IO ()
processRequest ctx req = do
    case requestMethod req of
        GET -> do
            pReq    <- parseRequest $ requestUri req
            manager <- newManager tlsManagerSettings
            rateLimit $ app_sched ctx
            resp    <- C.httpLbs pReq manager
            callback ctx (responseBody resp) (statusCode $ responseStatus resp)
            where callback = requestCallback req
        _ -> undefined