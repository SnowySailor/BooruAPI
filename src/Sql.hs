{-# LANGUAGE OverloadedStrings #-}
module Sql where

import qualified Database.PostgreSQL.Simple as P

-- Images
    -- Image data
    -- Image tags

insertImage :: P.Query
insertImage = undefined

insertImageTag :: P.Query
insertImageTag = "insert into image_tag (image_id, tag_id) values (?, ?)"

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

insertUserAward :: P.Query
insertUserAward = undefined

insertUserFavorite :: P.Query
insertUserFavorite = "insert into user_favorite (user_id, image_id) values (?, ?)"

-- Tags
    -- Tag definitions

insertTag :: P.Query
insertTag = undefined