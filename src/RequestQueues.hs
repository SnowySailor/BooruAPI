module RequestQueues where

import Datas
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Network.HTTP.Client as C
import Network.HTTP.Conduit
import Network.HTTP.Types.Status

-- Processing queues
processTBQueue :: a -> TBQueue b -> (a -> b -> IO c) -> Maybe (TMVar Int) -> IO c
processTBQueue rq q f mip = forever $ do
    bracket
        -- "Aquire": Get the next value from the queue
        (
            atomically $ do
                value <- readTBQueue q
                case mip of
                    Just ip -> do
                        orig <- takeTMVar ip
                        putTMVar ip $ orig+1
                    Nothing -> return ()
                return value
        )
        -- "Release": Decrement the count for in progress
        (
            \_ -> atomically $ do
                case mip of
                    Just ip -> do
                        orig <- takeTMVar ip
                        putTMVar ip $ orig-1
                    Nothing -> return ()
        )
        -- "In-between": Process the value and return the result
        (
            \toProcess -> f rq toProcess
        )

processTQueue :: a -> TQueue b -> (a -> b -> IO c) -> Maybe (TMVar Int) -> IO c
processTQueue rq q f mip = forever $ do
    bracket
        -- "Aquire": Get the next value from the queue
        (
            atomically $ do
                value <- readTQueue q
                case mip of
                    Just ip -> do
                        orig <- takeTMVar ip
                        putTMVar ip $ orig+1
                    Nothing -> return ()
                return value
        )
        -- "Release": Decrement the count for in progress
        (
            \_ -> atomically $ do
                case mip of
                    Just ip -> do
                        orig <- takeTMVar ip
                        putTMVar ip $ orig-1
                    Nothing -> return ()
        )
        -- "In-between": Process the value and return the result
        (
            \toProcess -> f rq toProcess
        )

processRequest :: RequestQueues -> QueueRequest -> IO ()
processRequest rq qreq = do
    case requestMethod qreq of
        GET -> do
            -- Establish request data
            pReq    <- parseRequest $ requestUri qreq
            manager <- newManager tlsManagerSettings
            -- Rate limit
            rateLimitM $ requestRateLimiter rq
            -- Perform request
            resp <- C.httpLbs pReq manager
            let qresp = QueueResponse { queueResponseBody = responseBody resp, queueResponseStatus = statusCode $ responseStatus resp }
            -- Issue the callback
            callback rq qreq qresp
            where callback = requestCallback qreq
        _ -> undefined -- TODO: Handle other methods

rateLimitM :: Maybe RateLimiter -> IO ()
rateLimitM (Just q) = atomically $ readTBQueue q
rateLimitM Nothing  = return ()

rateLimit :: RateLimiter -> IO ()
rateLimit q = atomically $ readTBQueue q

addTraverseToTBQueue :: (Traversable t) => t a -> TBQueue a -> TMVar b -> IO b
addTraverseToTBQueue l q c = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTBQueue q x
    -- Notify that we've completed the task
    atomically $ takeTMVar c

addTraverseToTQueue :: (Traversable t) => t a -> TQueue a -> TMVar b -> IO b
addTraverseToTQueue l q c = do
    -- Perform the write task
    forM_ l $ \x -> atomically $ writeTQueue q x
    -- Notify that we've completed the task
    atomically $ takeTMVar c

doRequestsMulti :: [QueueRequest] -> TVar a -> Maybe RateLimiter -> Int -> IO a
doRequestsMulti a b c d = doRequests' d a b c

doRequests :: [QueueRequest] -> TVar a -> Maybe RateLimiter -> IO a
doRequests = doRequests' 1

makeAndDoRequestsMulti :: (TVar a -> b -> QueueRequest) -> [b] -> TVar a -> Maybe RateLimiter -> Int -> IO a
makeAndDoRequestsMulti makeRequest l results rl t = 
    doRequests' t requests results rl
    where requests = map (makeRequest results) l

makeAndDoRequests :: (TVar a -> b -> QueueRequest) -> [b] -> TVar a -> Maybe RateLimiter -> IO a
makeAndDoRequests makeRequest l results =
    doRequests' 1 requests results
    where requests = map (makeRequest results) l

doRequests' :: Int -> [QueueRequest] -> TVar a -> Maybe RateLimiter -> IO a
doRequests' threadCount requests results rl = do
    -- Spawn new STM variables
    context <- atomically $ do
        requestQueue <- newTBQueue $ (*) 2 threadCount
        retryQueue   <- newTQueue
        inProgress   <- newTMVar 0
        return RequestQueues {
            requestQueuesMain  = requestQueue,
            requestQueuesRetry = retryQueue,
            requestRateLimiter = rl,
            requestInProgress  = inProgress
        }
    rCompleted   <- atomically $ newTMVar ()

    -- Create the threads
    addThread      <- forkIO $ addTraverseToTBQueue requests (requestQueuesMain context) rCompleted
    requestThreads <- replicateM threadCount $ forkIO $ processTBQueue context (requestQueuesMain context) processRequest $ Just (requestInProgress context)
    retryThreads   <- replicateM threadCount $ forkIO $ processTQueue context (requestQueuesRetry context) processRequest $ Just (requestInProgress context)
    let threads = [addThread] ++ requestThreads ++ retryThreads

    -- Wait for everything to be complete
    waiter <- async $ atomically $ do
        empty1  <- isEmptyTBQueue (requestQueuesMain context)
        empty2  <- isEmptyTQueue (requestQueuesRetry context)
        empty3  <- isEmptyTMVar rCompleted
        ipReq   <- takeTMVar (requestInProgress context)
        unless (all (==True) [empty1, empty2, empty3, (ipReq == 0)]) retry
        return ()
    catch
        (wait waiter)
        (\e -> do
            putStrLn $ show (e :: AsyncException)
            mapM_ killThread threads
        )
    mapM_ killThread threads
    atomically $ readTVar results
