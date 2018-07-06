{-# LANGUAGE OverloadedStrings #-}
module RequestQueues where

import Datas
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Client as C
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Processing

-- Rate limiting
dripSem :: (RealFrac a) => RateLimiter -> a -> IO ()
dripSem rl rate = forever $ do
     -- Write that we have a spot open
    atomically $ writeTBQueue rl ()
     -- Wait a certin amount of time before opening a new spot
    threadDelay $ (*) 1000000 $ floor $ 1/rate

rateLimitM :: Maybe RateLimiter -> IO ()
rateLimitM (Just q) = atomically $ readTBQueue q
rateLimitM Nothing  = return ()

rateLimit :: RateLimiter -> IO ()
rateLimit q = atomically $ readTBQueue q

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
            resp <- fmap Left (C.httpLbs pReq manager) `catch`
                (\ex -> case ex of
                    HttpExceptionRequest _ ResponseTimeout ->
                        return $ Right (pack $ show ex)
                    _ -> return $ Right (pack $ show ex))
            let qresp = case resp of
                            Left r -> QueueResponse { queueResponseBody = responseBody r, queueResponseStatus = statusCode $ responseStatus r }
                            Right e -> QueueResponse { queueResponseBody = e, queueResponseStatus = 9999 }
            -- Issue the callback
            callback rq qreq qresp
            where callback = requestCallback qreq
        _ -> undefined -- TODO: Handle other methods

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

retryRequest :: QueueRequest -> QueueResponse -> RequestQueues -> IO ()
retryRequest req resp rq = unless (requestTries req > requestTriesMax req) $
    atomically $
        writeTQueue (requestQueuesRetry rq) nr
        where nr = incReqeust req $ queueResponseStatus resp

incReqeust :: QueueRequest -> Int -> QueueRequest
incReqeust (QueueRequest u b m t c tm cb) s = QueueRequest u b m (t+1) (s:c) tm cb
