module DerpAPI where

import Datas
import Helpers
import APIGetter
import DataManipulation
import Network.URI.Encode
import Control.Exception
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Processing

-- Images

getImage :: ImageId -> Settings -> IO (Image, Int)
getImage i s = do
    (imageData, status) <- getImageJSON i s
    return (decodeNoMaybe imageData, status)

bsToImage :: ByteString -> Image
bsToImage = decodeNoMaybe

bsToUser :: ByteString -> User
bsToUser = decodeNoMaybe

getImageCommentsLimited :: ImageId -> Int -> AppContext -> IO [Comment]
getImageCommentsLimited i count ctx = do
    results      <- atomically $ newTVar []
    let (p, _)      = divMod count $ comments_per_page $ app_sett ctx
        requests    = map (makeCommentPageRequest results (app_sett ctx) i) [1..p]
        threadCount = num_request_threads $ app_sett ctx
    requestQueue <- atomically $ newTQueue
    retryQueue   <- atomically $ newTQueue
    addTraverseToTQueueSync requests requestQueue
    requestThreads <- replicateM threadCount $ forkIO $ processTQueue ctx (\_ -> requestQueue) $ processRequest 
    retryThreads   <- replicateM threadCount $ forkIO $ processTQueue ctx (\_ -> retryQueue) $ processRequest
    let threads = requestThreads ++ retryThreads
    waiter <- async $ atomically $ do
        empty1 <- isEmptyTQueue requestQueue
        empty2 <- isEmptyTQueue retryQueue
        unless (all (==True) [empty1, empty2]) retry
        return ()
    catch
        (wait waiter)
        (\e -> do
            atomically $ writeTQueue (schedOut $ app_sched ctx) $ show (e :: AsyncException)
            mapM_ killThread threads
        )
    mapM_ killThread threads
    atomically $ readTVar results

handleCommentPageResponse' :: TVar [a] -> AppContext -> ByteString -> Int -> IO ()
handleCommentPageResponse' = undefined

makeCommentPageRequest :: TVar [a] -> Settings -> ImageId -> PageNo -> AppRequest
makeCommentPageRequest t s i p = AppRequest uri Nothing GET $ handleCommentPageResponse' t
    where uri = commentsAPI i p s

getImageCommentsSimple :: ImageId -> Int -> Settings -> IO [Comment]
getImageCommentsSimple i count s = do
    let (p, _) = divMod count $ comments_per_page s
    comments <- mapM (\x -> getCommentPage i x s) [1..(p+1)]
    return . flatten . map (\(CommentPage c) -> c) $ filterNulls $ map fst comments

-- Comments

getCommentPage :: ImageId -> PageNo -> Settings -> IO (CommentPage, Int)
getCommentPage i p s = do
    (json, status) <- getCommentsJSON i p s
    return $ (decodeNoMaybe json, status)

-- Users

getUser :: UserId -> Settings -> IO (User, Int)
getUser i s = do
    (json, status) <- getUserJSON i s
    return $ (decodeNoMaybe json, status)

getUserByName :: Username -> Settings -> IO (User, Int)
getUserByName n s = do
    (json, status) <- getUserJSON (encode n) s
    return $ (decodeNoMaybe json, status)

getUserFavorites :: Username -> Settings -> IO [ImageId]
getUserFavorites u s = do
    (firstPage, status)  <- getSearchPage q 1 s
    totalCount <- return $ case firstPage of
        NullSearchPage -> 0
        SearchPage c _ -> c
    let (p, _) = divMod totalCount $ images_per_page s
    userFaves  <- mapM (\x -> getSearchPage q x s) [2..(p+1)]
    return . map getImageId . flatten . map getSearchImages $ firstPage:(filterNulls $ map fst userFaves)
    where q = "faved_by:" ++ u

-- Tags

getAllTags :: PageNo -> Settings -> IO [Tag]
getAllTags maxPage s = do
    tagPages <- mapM (\x -> getTagPage x s) [1..maxPage]
    return . filterNulls . flatten $ map getTagPageTags $ map fst tagPages

getTagPage :: PageNo -> Settings -> IO (TagPage, Int)
getTagPage p s = do
    (json, status) <- getTagsJSON p s
    return $ (decodeNoMaybe json, status)

-- Search

getSearchPage :: String -> Int -> Settings -> IO (SearchPage, Int)
getSearchPage q i s = do
    (json, status) <- getSearchJSON q i s
    return $ (decodeNoMaybe json, status)
