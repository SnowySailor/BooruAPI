module Main where

import Control.Concurrent.Async
import Control.Exception (AsyncException)
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent
import Control.Concurrent.STM
import Datas
import Database.Pool
import Config
import Processing
import RequestQueues
import Helpers
import DerpCloner

main :: IO ()
main = do
    -- -- START SETUP --
    -- Get config file data
    settings <- getSettings
    creds    <- getDatabaseCreds

    -- Start the connection pool
    resource <- defaultResources
    pool     <- getPool resource (db_database creds)

    -- Create new rate-limit, output, and results
    rl      <- atomically $ newTBQueue $ (*) 2 $ num_request_threads settings
    out     <- atomically $ newTQueue
    results <- atomically $ newTVar []

    -- Create necessary data
    let imageList    = map (makeImageRequest settings pool (db_schema creds) out)   [(load_image_start settings)..(load_image_end settings)]
        userList     = map (makeUserRequest settings pool (db_schema creds) out)    [(load_user_start settings)..(load_user_end settings)]
        tagList      = map (makeTagPageRequest settings pool (db_schema creds) out) [(load_tags_start settings)..(load_tags_end settings)]
        combinedList = zipLists tagList $ zipLists imageList userList -- [tag, image, tag, user, tag, image, tag, user, tag, image, etc.]
        threadCount  = num_request_threads settings

    -- START TASKS --

    -- Flags for when outputting is complete
    oComplete <- atomically $ newTMVar 0

    -- Start all necessary loader/output/rate-limiting threads
    dripSemThread <- forkIO $ dripSem rl (requests_per_second settings)
    outThread     <- forkIO $ processTQueue () out handleOut $ Just oComplete

    -- Being program
    writeOut out "Running..."
    _ <- doRequestsMulti combinedList results (Just rl) threadCount
    
    -- END START TASKS --

    let threads = [outThread, dripSemThread]

    -- Create async waiter
    waiter <- async $ atomically $ do
        ipOut  <- takeTMVar oComplete
        -- If all are empty, then the run is complete
        unless (all (==True) [(ipOut==0)]) retry
        return ()

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

-- Handling
handleOut :: () -> String -> IO ()
handleOut _ s = putStrLn s
