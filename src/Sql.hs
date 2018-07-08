{-# LANGUAGE OverloadedStrings #-}
module Sql where

import Database.PostgreSQL.Simple.Types
import Data.ByteString.Char8

-- Images
    -- Image data
        -- Standard images
        -- Duplicate images
        -- Deleted images
    -- Image tags

insertImage :: String -> Query
insertImage schema = Query $ append "insert into " $ append (pack schema) ".image (id, uploader_id, description, upvotes, downvotes, faves, score, comment_count, created_at, updated_at, first_seen_at, width, height, aspect_ratio) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertImageDuplicate :: String -> Query
insertImageDuplicate schema = Query $ append "insert into " $ append (pack schema) ".image_duplicate (image_id, duplicate_of_id, uploader_id, created_at, updated_at, first_seen_at) values (?, ?, ?, ?, ?, ?)"

insertImageDeleted :: String -> Query
insertImageDeleted schema = Query $ append "insert into " $ append (pack schema) ".image_deletion (image_id, uploader_id, deletion_reason, created_at, updated_at, first_seen_at) values (?, ?, ?, ?, ?, ?)"

insertImageTag :: String -> Query
insertImageTag schema = Query $ append "insert into " $ append (pack schema) ".image_tag (image_id, tag_id) values (?, ?) on conflict do nothing"

-- Comments
    -- Comments

insertComment :: String -> Query
insertComment schema = Query $ append "insert into " $ append (pack schema) ".image_comment (id, image_id, author, body, posted_at, deleted) values (?, ?, ?, ?, ?, ?) on conflict do nothing"

-- Users
    -- Profiles
    -- Awards
    -- Links
    -- Favorites

insertUser :: String -> Query
insertUser schema = Query $ append "insert into " $ append (pack schema) ".user (id, name, description, role, created_at, comment_count, uploads_count, post_count, topic_count) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertUserAward :: String -> Query
insertUserAward schema = Query $ append "insert into " $ append (pack schema) ".user_award (id, user_id, title, label, date) values (?, ?, ?, ?, ?) on conflict do nothing"

insertUserLink :: String -> Query
insertUserLink schema = Query $ append "insert into " $ append (pack schema) ".user_link (user_id, tag_id, date, state) values (?, ?, ?, ?) on conflict do nothing"

insertUserFavorite :: String -> Query
insertUserFavorite schema = Query $ append "insert into " $ append (pack schema) ".user_favorite (user_id, image_id) values (?, ?) on conflict do nothing"

-- Tags
    -- Tag definitions
    -- Implied tags

insertTag :: String -> Query
insertTag schema = Query $ append "insert into " $ append (pack schema) ".tag (id, name, slug, description, short_description, aliased_to_id, category, spoiler_image) values (?, ?, ?, ?, ?, ?, ?, ?)"

insertTagImplication :: String -> Query
insertTagImplication schema = Query $ append "insert into " $ append (pack schema) ".tag_implication (tag_id, implied_tag_id) values (?, ?) on conflict do nothing"
