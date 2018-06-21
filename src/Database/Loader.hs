module Database.Loader where

import Sql
import Datas
import GHC.Int
import Database.Logger
import Database.PostgreSQL.Simple as P

loadUser :: User -> IO [Int64]
loadUser u = undefined

loadUserFavorites :: UserFull -> Connection -> IO Int64
loadUserFavorites (UserFull u f) conn = do
    executeMany conn insertUserFavorite faves
    where faves = map (\x -> ((user_id u), x)) f
loadUserFavorites (AnonymousUserFull) _ = do
    logError "loadUserFavorites called on AnonymousUserFull"
    return 0
loadUserFavorites (NullUserFull) _ = do
    logError "loadUserFavorites called on NullUserFull"
    return 0

loadImageTags :: Image -> Connection -> IO Int64
loadImageTags (Image d) conn = do
    executeMany conn insertImageTag tags
    where tags = map (\x -> ((image_id d), x)) $ image_tags d
loadImageTags (ImageDuplicate _) _ = do
    logError "loadImageTags called on ImageDuplicate"
    return 0
loadImageTags (ImageDeleted _) _ = do
    logError "loadImageTags called on ImageDeleted"
    return 0
loadImageTags (NullImage) _ = do
    logError "loadImageTags called on NullImage"
    return 0
