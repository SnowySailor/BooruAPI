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
--import Control.Concurrent.STM.MonadIO (modifyTMVar_)

data RequestQueues = RequestQueues {
    requestQueuesMain  :: TBQueue QueueRequest,
    requestQueuesRetry :: TQueue QueueRequest,
    requestRateLimiter :: Maybe RateLimiter,
    requestInProgress  :: TMVar Int
}

data QueueResponse = QueueResponse {
    queueResponseBody   :: ByteString,
    queueResponseStatus :: Int
}

data QueueRequest = QueueRequest {
    requestUri      :: String,
    requestBody     :: Maybe ByteString,
    requestMethod   :: HTTPMethod,
    requestTries    :: Int,
    requestCodes    :: [Int],
    requestTriesMax :: Int,
    requestCallback :: RequestQueues -> QueueRequest -> QueueResponse -> IO ()
}

type RateLimiter = TBQueue ()

-- Processing queues
processTBQueue :: RequestQueues -> TBQueue a -> (RequestQueues -> a -> IO b) -> IO b
processTBQueue rq q f = forever $ do
    bracket
        -- "Aquire": Get the next value from the queue
        (
            atomically $ do
                value <- readTBQueue q
                orig <- takeTMVar (requestInProgress rq)
                putTMVar (requestInProgress rq) $ orig+1
                return value
        )
        -- "Release": Decrement the count for in progress
        (
            \_ -> atomically $ do
                orig <- takeTMVar (requestInProgress rq)
                putTMVar (requestInProgress rq) $ orig-1
        )
        -- "In-between": Process the value and return the result
        (
            \toProcess -> f rq toProcess
        )

processTQueue :: RequestQueues -> TQueue a -> (RequestQueues -> a -> IO b) -> IO b
processTQueue rq q f = forever $ do
    bracket
        -- "Aquire": Get the next value from the queue
        (
            atomically $ do
                value <- readTQueue q
                orig <- takeTMVar (requestInProgress rq)
                putTMVar (requestInProgress rq) $ orig+1
                return value
        )
        -- "Release": Decrement the count for in progress
        (
            \_ -> atomically $ do
                orig <- takeTMVar (requestInProgress rq)
                putTMVar (requestInProgress rq) $ orig-1
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

doRequestsMulti :: [QueueRequest] -> TMVar a -> Maybe RateLimiter -> Int -> IO a
doRequestsMulti a b c d = doRequests' d a b c

doRequests :: [QueueRequest] -> TMVar a -> Maybe RateLimiter -> IO a
doRequests = doRequests' 1

makeAndDoRequestsMulti :: (TMVar a -> b -> QueueRequest) -> [b] -> TMVar a -> Maybe RateLimiter -> Int -> IO a
makeAndDoRequestsMulti makeRequest l results rl t = 
    doRequests' t requests results rl
    where requests = map (makeRequest results) l

makeAndDoRequests :: (TMVar a -> b -> QueueRequest) -> [b] -> TMVar a -> Maybe RateLimiter -> IO a
makeAndDoRequests makeRequest l results =
    doRequests' 1 requests results
    where requests = map (makeRequest results) l

doRequests' :: Int -> [QueueRequest] -> TMVar a -> Maybe RateLimiter -> IO a
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
    requestThreads <- replicateM threadCount $ forkIO $ processTBQueue context (requestQueuesMain context) processRequest
    retryThreads   <- replicateM threadCount $ forkIO $ processTQueue context (requestQueuesRetry context) processRequest
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
    atomically $ readTMVar results
