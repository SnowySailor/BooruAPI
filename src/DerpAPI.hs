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

-- Images

getImage :: ImageId -> Settings -> IO (Image, Int)
getImage i s = do
    (imageData, status) <- getImageJSON i s
    return (decodeNoMaybe imageData, status)

getImageComments :: ImageId -> Int -> AppContext -> IO [CommentPage]
getImageComments i count ctx = do
    let (p, _) = divMod count $ comments_per_page $ app_sett ctx
    getNestedRequests (\x y -> makeCommentPageRequest x y i) [1..p] ctx

-- Comments

handleCommentPageResponse :: TVar [CommentPage] -> AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleCommentPageResponse t ctx req retry bs s = 
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
makeCommentPageRequest t s i p = AppRequest uri Nothing GET 0 [] $ handleCommentPageResponse t
    where uri = commentsAPI i p s

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

handleSearchPageResponse :: TVar [SearchPage] -> AppContext -> AppRequest -> Maybe (TQueue AppRequest) -> ByteString -> Int -> IO ()
handleSearchPageResponse t ctx req retry bs s = do
    if s >= 200 && s < 300 then do
        atomically $ do
            let page = decodeNoMaybe bs
            v <- readTVar t
            writeTVar t $ page:v
    else do
        case retry of
            Just a -> addToTQueue (incReqeust req s) a
            Nothing -> return ()

getUserFavorites :: Username -> AppContext -> IO [ImageId]
getUserFavorites u ctx = do
    x <- getNestedRequests (\x y -> makeSearchPageRequest x y q) [1] ctx
    case x of
        []  -> return []
        x:_ ->
            case x of
                NullSearchPage -> do
                    writeOut ctx $ "Got NullSearchPage for first page"
                    return []
                SearchPage c _ -> do
                    let totalCount = c
                        (p, _) = divMod totalCount $ (images_per_page . app_sett) ctx
                    restOfUserFaves <- getNestedRequests (\x y -> makeSearchPageRequest x y q) [2..p] ctx
                    return . map getImageId . flatten . map getSearchImages $ x:(filterNulls $ restOfUserFaves)
    where q = "faved_by:" ++ u

makeSearchPageRequest :: TVar [SearchPage] -> Settings -> String -> PageNo -> AppRequest
makeSearchPageRequest t s q p = AppRequest uri Nothing GET 0 [] $ handleSearchPageResponse t
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
