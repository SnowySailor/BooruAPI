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
dripSem :: Scheduler -> IO ()
dripSem sched = forever $ do
     -- Write that we have a spot open
    atomically $ writeTBQueue (schedRateLimiter sched) ()
     -- Wait a certin amount of time before opening a new spot
    threadDelay $ (*) 1000000 $ floor $ 1/(schedReqPerSec sched)

rateLimit :: Scheduler -> IO ()
rateLimit sched = atomically $ readTBQueue $ schedRateLimiter sched

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

writeTBQueueM :: Maybe (TQueue a) -> a -> IO ()
writeTBQueueM mQ e =
    case mQ of
        Just q -> atomically $ writeTBQueue q e
        Nothing -> return ()


-- Processing queues
processTBQueue :: a -> (a -> TBQueue b) -> (a -> b -> Maybe (TQueue b) -> IO c) -> IO c
processTBQueue sched q f = forever $ do
     -- Get the next value from the queue
    toProcess <- atomically $ readTBQueue (q sched)
     -- Process the value and return the result
    f sched toProcess Nothing

processTQueue :: a -> (a -> TQueue b) -> (a -> b -> Maybe (TQueue b) -> IO c) -> IO c
processTQueue sched q f = forever $ do
     -- Get the next value from the queue
    toProcess <- atomically $ readTQueue q
     -- Process the value and return the result
    f sched toProcess Nothing

processTQueueRetry :: a -> (a -> TQueue b) -> (a -> b -> Maybe (TQueue b) -> IO c) -> TQueue b -> IO c
processTQueueRetry sched q f ret = forever $ do
     -- Get the next value from the queue
    toProcess <- atomically $ readTQueue q
     -- Process the value and return the result
    f sched toProcess $ Just ret

retryRequest :: AppRequest -> AppContext -> IO ()
retryRequest r c = unless (requestTries r > 3) $ atomically $ writeTQueue (schedRetryQueue $ app_sched c) r

incReqeust :: AppRequest -> Int -> AppRequest
incReqeust (AppRequest u b m t c cb) s = AppRequest u b m (t+1) (s:c) cb

-- Processing AppRequests
processRequest :: AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> IO ()
processRequest ctx req retry = do
    case requestMethod req of
        GET -> do
            pReq    <- parseRequest $ requestUri req
            manager <- newManager tlsManagerSettings
            rateLimit $ app_sched ctx
            resp    <- C.httpLbs pReq manager
            writeOut ctx $ responseBody resp
            callback ctx req retry (responseBody resp) (statusCode $ responseStatus resp)
            where callback = requestCallback req
        _ -> undefined -- TODO: Handle other methods

getNestedRequests :: (TVar [a] -> Settings -> c -> AppRequest) -> [c] -> AppContext -> IO [a]
getNestedRequests makeRequest applyList ctx = do
    results      <- atomically $ newTVar []
    let requests    = map (makeRequest results (app_sett ctx)) applyList
        threadCount = num_request_threads $ app_sett ctx
    mapM_ (writeOut ctx) $ map requestUri requests
    requestQueue <- atomically $ newTQueue
    retryQueue   <- atomically $ newTQueue
    addTraverseToTQueueSync requests requestQueue
    requestThreads <- replicateM threadCount $ forkIO $ processTQueueRetry ctx (\_ -> requestQueue) (processRequest) retryQueue
    retryThreads   <- replicateM threadCount $ forkIO $ processTQueueRetry ctx (\_ -> retryQueue) (processRequest) retryQueue
    let threads = requestThreads ++ retryThreads
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTQueue requestQueue
        empty2 <- isEmptyTQueue retryQueue
        unless (all (==True) [empty1, empty2]) retry
        return ()
    catch
        (wait waiter)
        (\e -> do
            writeOut ctx (e :: AsyncException)
            mapM_ killThread threads
        )
    mapM_ killThread threads
    atomically $ readTVar results
