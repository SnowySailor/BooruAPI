{-# LANGUAGE OverloadedStrings #-}
module Sql where

import qualified Database.MySQL.Simple as M

friendSQL :: M.Query
friendSQL = "select u.user_id,full_name,user_name,user_image,is_invisible,cclastactivity from phpfox_user u join phpfox_friend f on f.friend_user_id = u.user_id where f.user_id=? and f.is_page=0 order by u.user_id"

friendActive :: M.Query
friendActive = "select u.user_id,cclastactivity from phpfox_user u join phpfox_friend f on f.friend_user_id = u.user_id where f.user_id=? and f.is_page=0"

userSQL :: M.Query
userSQL = "select user_id,full_name,user_name,user_image,is_invisible,cclastactivity from phpfox_user where user_id=?"


getNowUnix :: M.Query
getNowUnix = "select unix_timestamp()"

getNowISO8601 :: M.Query
getNowISO8601 = "select DATE_FORMAT(NOW(), '%Y-%m-%dT%H:%i:%sZ')"

recentSingleSources :: M.Query
recentSingleSources = "select distinct from_id from new_im_storage where is_seen = 0 or time_stamp > unix_timestamp() - 60*60*24 and to_id = ?"
-- Get sender id's. Then we'll just iterate over that..
-- Not efficient in SQL standards, but it will provide the most correct solution
-- with MySQL involved.

singleSourceGet :: M.Query
singleSourceGet = "select id, from_id, to_id, message, DATE_FORMAT(FROM_UNIXTIME(time_stamp), '%Y-%m-%dT%H:%i:%sZ') from new_im_storage where (from_id = ? and to_id = ?) or (to_id = ? and from_id = ?) order by time_stamp desc limit ?"
-- This should give us an ISO 8601 Timestamp string.

updateLive :: M.Query
updateLive = "update phpfox_user set cclastactivity = greatest(unix_timestamp() - ?,cclastactivity) where user_id = ?"

addSingleMessage :: M.Query
addSingleMessage = "insert into new_im_storage (from_id, to_id, message, time_stamp) values (?,?,?, unix_timestamp())"