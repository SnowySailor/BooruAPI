module DerpAPI where

import Datas
import Helpers
import APIGetter
import DataHelpers
import Network.URI.Encode
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent
import Control.Monad
import Processing
import RequestQueues

-- Images

getImage :: ImageId -> Settings -> IO (Image, Int)
getImage i s = do
    (imageData, status) <- getImageJSON i s
    return (decodeNoMaybe imageData, status)

-- Comments

getImageComments :: ImageId -> Int -> Settings -> TQueue String -> IO [CommentPage]
getImageComments a b c d = getImageComments' a b c d Nothing

getImageCommentsRL :: ImageId -> Int -> Settings -> TQueue String -> RateLimiter -> IO [CommentPage]
getImageCommentsRL a b c d e = getImageComments' a b c d $ Just e

getImageComments' :: ImageId -> Int -> Settings -> TQueue String -> Maybe RateLimiter -> IO [CommentPage]
getImageComments' i count s out rl = do
    results <- atomically $ newTVar []
    let (p, _) = divMod count $ comments_per_page s
        requests = map (\page -> makeCommentPageRequest s out i page results) [1..p]
    doRequests requests results rl

handleCommentPageResponse :: TQueue String -> TVar [CommentPage] -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleCommentPageResponse out results rq req resp = 
    if status >= 200 && status < 300 then do
        atomically $ do
            let page = decodeNoMaybe $ queueResponseBody resp
            v <- readTVar results
            writeTVar results $ page:v
    else do
        handleBadResponse out rq req resp
    where status = queueResponseStatus resp

makeCommentPageRequest :: Settings -> TQueue String -> ImageId -> PageNo -> TVar [CommentPage] -> QueueRequest
makeCommentPageRequest s out image page results = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleCommentPageResponse out results
    where uri = commentsAPI image page s

-- Simple comments

getImageCommentsSimple :: ImageId -> Int -> Settings -> IO [Comment]
getImageCommentsSimple i count s = do
    let (p, _) = divMod count $ comments_per_page s
    comments <- mapM (\x -> getCommentPage i x s) [1..(p+1)]
    return . flatten . map (\(CommentPage c) -> c) $ filterNulls $ map fst comments

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

getUserFavorites :: Username -> Settings -> TQueue String -> IO [ImageId]
getUserFavorites a b c = getUserFavorites' a b c Nothing

getUserFavoritesRL :: Username -> Settings -> TQueue String -> RateLimiter -> IO [ImageId]
getUserFavoritesRL a b c d = getUserFavorites' a b c $ Just d

getUserFavorites' :: Username -> Settings -> TQueue String -> Maybe RateLimiter -> IO [ImageId]
getUserFavorites' name s out rl = do
    results <- atomically $ newTVar []

    let fReq = [makeSearchPageRequest s out q 1 results]
    firstPage <- doRequests fReq results rl
    case firstPage of
        []  -> return []
        page:_ ->
            case page of
                NullSearchPage -> do
                    writeOut out "Got NullSearchPage for first page"
                    return []
                SearchPage c _ -> do
                    let totalCount = c
                        (p, _) = divMod totalCount $ images_per_page s
                        requests = map (\p -> makeSearchPageRequest s out q p results) [2..p]
                    restOfUserFaves <- doRequests requests results rl
                    return . map getImageId . flatten . map getSearchImages $ page:(filterNulls $ restOfUserFaves)
    where q = "faved_by:" ++ name

handleSearchPageResponse :: TQueue String -> TVar [SearchPage] -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleSearchPageResponse out results rq req resp = do
    if status >= 200 && status < 300 then do
        atomically $ do
            let page = decodeNoMaybe $ queueResponseBody resp
            v <- readTVar results
            writeTVar results $ page:v
    else do
        handleBadResponse out rq req resp
    where status = queueResponseStatus resp

makeSearchPageRequest :: Settings -> TQueue String -> String -> PageNo -> TVar [SearchPage] -> QueueRequest
makeSearchPageRequest s out q p results = QueueRequest uri Nothing GET 0 [] (max_retry_count s) $ handleSearchPageResponse out results
    where uri = searchAPI q p s

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

handleBadResponse :: TQueue String -> RequestQueues -> QueueRequest -> QueueResponse -> IO ()
handleBadResponse out rq req resp = do
    if status >= 300 && status < 400 then
        writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    else if status >= 400 && status < 500 then
        case status of
            400 -> unless (requestTries req >= max_retries) $ do
                        writeOut out $ "Got 400 at " ++ (requestUri req) ++ ". Retrying."
                        retryRequest req resp rq
            _   -> writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    else if status >= 500 && status < 600 then
        case status of
            500 -> unless (requestTries req >= max_retries) $ do
                        writeOut out $ "Got 500 at " ++ (requestUri req) ++ ". Retrying."
                        retryRequest req resp rq
            _   -> writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    else
        writeOut out $ "Got " ++ show status ++ " at " ++ (requestUri req) ++ ". Not retrying."
    where max_retries = requestTriesMax req
          status = queueResponseStatus resp