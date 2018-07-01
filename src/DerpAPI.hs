module DerpAPI where

import Datas
import Helpers
import APIGetter
import DataHelpers
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

getImageComments :: ImageId -> Int -> AppContext -> IO [CommentPage]
getImageComments i count ctx = do
    let (p, _) = divMod count $ comments_per_page $ app_sett ctx
    getNestedRequests (\x y -> makeCommentPageRequest x y i) [1..p] ctx
    -- results      <- atomically $ newTVar []
    -- let (p, _)      = divMod count $ comments_per_page $ app_sett ctx
    --     requests    = map (makeCommentPageRequest results (app_sett ctx) i) [1..p]
    --     threadCount = num_request_threads $ app_sett ctx
    -- requestQueue <- atomically $ newTQueue
    -- retryQueue   <- atomically $ newTQueue
    -- addTraverseToTQueueSync requests requestQueue
    -- requestThreads <- replicateM threadCount $ forkIO $ processTQueueRetry ctx (\_ -> requestQueue) (processRequest) retryQueue
    -- retryThreads   <- replicateM threadCount $ forkIO $ processTQueueRetry ctx (\_ -> retryQueue) (processRequest) retryQueue
    -- let threads = requestThreads ++ retryThreads
    -- waiter <- async $ atomically $ do
    --     empty1 <- isEmptyTQueue requestQueue
    --     empty2 <- isEmptyTQueue retryQueue
    --     unless (all (==True) [empty1, empty2]) retry
    --     return ()
    -- catch
    --     (wait waiter)
    --     (\e -> do
    --         writeOut ctx (e :: AsyncException)
    --         mapM_ killThread threads
    --     )
    -- mapM_ killThread threads
    -- atomically $ readTVar results

getNestedRequests :: (TVar [a] -> Settings -> c -> AppRequest) -> [c] -> AppContext -> IO [a]
getNestedRequests makeRequest applyList ctx = do
    results      <- atomically $ newTVar []
    let requests    = map (makeRequest results (app_sett ctx)) applyList
        threadCount = num_request_threads $ app_sett ctx
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

handleCommentPageResponse' :: TVar [CommentPage] -> AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleCommentPageResponse' t ctx req retry bs s = 
    if s >= 200 && s < 300 then do
        atomically $ do
            let page = decodeNoMaybe bs
            v <- readTVar t
            writeTVar t $ page:v
    else do
        case retry of
            Just a -> addToTQueue (incReqeust req s) a
            Nothing -> return ()


makeCommentPageRequest :: TVar [CommentPage] -> Settings -> ImageId -> PageNo -> AppRequest
makeCommentPageRequest t s i p = AppRequest uri Nothing GET 0 [] $ handleCommentPageResponse' t
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

getUserFavoritesSimple :: Username -> Settings -> IO [ImageId]
getUserFavoritesSimple u s = do
    (firstPage, status)  <- getSearchPage q 1 s
    totalCount <- return $ case firstPage of
        NullSearchPage -> 0
        SearchPage c _ -> c
    let (p, _) = divMod totalCount $ images_per_page s
    userFaves  <- mapM (\x -> getSearchPage q x s) [2..(p+1)]
    return . map getImageId . flatten . map getSearchImages $ firstPage:(filterNulls $ map fst userFaves)
    where q = "faved_by:" ++ u

-- Tags

getAllTagsSimple :: PageNo -> Settings -> IO [Tag]
getAllTagsSimple maxPage s = do
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
