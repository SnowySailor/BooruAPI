{-# LANGUAGE FlexibleContexts #-}
module Database.Pool where

import Control.Monad.Trans.Control
import Control.Concurrent
import Data.Pool
import Database.PostgreSQL.Simple
import Config
import Datas

import qualified Data.Map as M

createPool' :: ServerResources -> String -> IO (Pool Connection)
createPool' rs dbname = do
    info <- getConnectInfo
    p    <- createPool (connect $ info {connectDatabase = dbname}) close 5 10 5
    return p

defaultResources :: IO ServerResources
defaultResources = do
    ps   <- newMVar (M.empty)
    lock <- newEmptyMVar
    return $ ServerResources {
        serverPools    = ps,
        serverPoolLock = lock
    }

getPool :: ServerResources -> String -> IO (Pool Connection)
getPool rs db_name = do
    poolmap <- readMVar (serverPools rs)

    case M.lookup db_name poolmap of
        Just p  -> return p
        Nothing -> do
            -- Take the lock, no one else can double put.
            lock <- putMVar (serverPoolLock rs) ()
            -- In case another did it before us,
            -- check that we don't overwrite it.
            pm   <- readMVar (serverPools rs)
            cn   <- case M.lookup db_name pm of
                Just p  -> do
                    -- We were not expecting this!
                    -- Either way, do what we did above.
                    return p
                Nothing -> do
                    newPool <- createPool' rs db_name
                    modifyMVar_ (serverPools rs) $ \ps -> return $ M.insert db_name newPool ps
                    return newPool
            -- Unlock now that we have gotten a connection to work with
            _ <- takeMVar (serverPoolLock rs)
            -- Finally return the connection we have gotten
            return cn

withPool :: (MonadBaseControl IO m) => Pool t -> (t -> m b) -> m b
withPool p f = withResource p $ \conn -> f conn
