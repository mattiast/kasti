{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module EpisodeDb where
import Database.SQLite.Simple
import Data.Function((&))
import Types

feedsFromDB :: Connection -> IO [(FeedId, FeedInfo)]
feedsFromDB conn = query_ conn "select id, name, url from feeds"
    & fmap (fmap $ \((Only fid) :. fi) -> (fid, fi))

episodesFromDB :: FeedId -> Connection -> IO [(EpisodeId, Episode)]
episodesFromDB fid conn = query conn "select id, url, title, date from episodes where feed_id = ?" (Only fid)
    & fmap (fmap $ \((Only eid) :. ep) -> (eid, ep))

getFeedInfo :: FeedId -> Connection -> IO FeedInfo
getFeedInfo fid conn = do
    -- TODO this is bad, fix the assumption
    [fi :: FeedInfo] <- query conn "select name, url from feeds where id = ?" (Only fid)
    return fi

writeEpisodes :: Connection -> FeedId -> [Episode] -> IO ()
writeEpisodes conn fid eps =
    executeMany conn "insert or ignore into episodes(feed_id, url, title, date) values (?,?,?,?)"
            [ Only fid :. ep | ep <- eps ]

writeFeeds :: Connection -> [FeedInfo] -> IO ()
writeFeeds conn fis =
    executeMany conn "insert into feeds(name, url) values (?,?)" fis

withConn :: (Connection -> IO a) -> IO a
withConn = withConnection "db.sqlite"

savePosition :: ProgressMsg -> Connection -> IO ()
savePosition msg conn = do
    execute conn "insert or ignore into progress(episode_id, position) values (?, ?)" msg
    execute conn "update progress set position = ? where episode_id = ?" (proPos msg, prEpId msg)

getPosition :: EpisodeId -> Connection -> IO Double
getPosition eid conn = do
    (poss :: [Double]) <- query conn "select position from progress where episode_id = ?" (Only eid)
        & fmap (map fromOnly)

    case poss of
        [] -> return 0
        [pos] -> return pos
        _ -> fail $ "weird, position not unique for" ++ show eid
