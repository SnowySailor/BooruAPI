{-# LANGUAGE OverloadedStrings #-}
module Sql where

import qualified Database.PostgreSQL.Simple as P

-- Images
    -- Image data
        -- Standard images
        -- Duplicate images
        -- Deleted images
    -- Image tags

insertImage :: P.Query
insertImage = "insert into image (id, uploader_id, description, upvotes, downvotes, faves, score, comment_count, created_at, updated_at, first_seen_at, width, height, aspect_ratio) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertImageDuplicate :: P.Query
insertImageDuplicate = "insert into image_duplicate (id, duplicate_of_id, uploader_id, created_at, updated_at, first_seen_at) values (?, ?, ?, ?, ?, ?)"

insertImageDeleted :: P.Query
insertImageDeleted = "insert into image_deleted (id, uploader_id, deletion_reason, created_at, updated_at, first_seen_at) values (?, ?, ?, ?, ?, ?)"

insertImageTag :: P.Query
insertImageTag = "insert into image_tag (image_id, tag_id) values (?, ?)"

-- Comments
    -- Comments

insertComment :: P.Query
insertComment = "insert into image_comment (id, image_id, author_id, body, posted_at, deleted) values (?, ?, ?, ?, ?, ?)"

-- Users
    -- Profiles
    -- Awards
    -- Links
    -- Favorites

insertUser :: P.Query
insertUser = "insert into user (id, name, description, role, created_at, comment_count, uploads_count, post_count, topic_count) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertUserAward :: P.Query
insertUserAward = "insert into user_award (id, user_id, title, label, date) values (?, ?, ?, ?, ?)"

insertUserLink :: P.Query
insertUserLink = "insert into user_link (user_id, tag_id) values (?, ?)"

insertUserFavorite :: P.Query
insertUserFavorite = "insert into user_favorite (user_id, image_id) values (?, ?)"

-- Tags
    -- Tag definitions
    -- Implied tags

insertTag :: P.Query
insertTag = "insert into tag (id, name, slug, description, short_description, aliased_to_id, category, spoiler_image) values (?, ?, ?, ?, ?, ?, ?, ?)"

insertTagImplication :: P.Query
insertTagImplication = "insert into tag_implication (tag_id, implied_tag_id) values (?, ?)"
