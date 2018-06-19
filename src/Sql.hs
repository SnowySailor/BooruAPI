{-# LANGUAGE OverloadedStrings #-}
module Sql where

import qualified Database.PostgreSQL.Simple as P

-- Images
    -- Image data
    -- Image tags

insertImage :: P.Query
insertImage = undefined

insertImageTag :: P.Query
insertImageTag = undefined

-- Comments
    -- Comments

insertComment :: P.Query
insertComment = undefined

-- Users
    -- Profiles
    -- Awards
    -- Favorites

insertUser :: P.Query
insertUser = undefined

insertUserAwards :: P.Query
insertUserAwards = undefined

insertUserFavorites :: P.Query
insertUserFavorites = undefined

-- Tags
    -- Tag definitions

insertTag :: P.Query
insertTag = undefined