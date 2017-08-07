{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module EpisodeDb where
import Database.SQLite.Simple
import Data.Function((&))
import Data.Maybe(listToMaybe)
import Types

readFeeds :: Connection -> IO [(FeedId, FeedInfo)]
readFeeds conn = query_ conn "select id, name, url from feeds"
    & fmap (fmap $ \((Only fid) :. fi) -> (fid, fi))

readEpisodes :: FeedId -> Connection -> IO [(EpisodeId, Episode)]
readEpisodes fid conn = query conn "select id, url, title, date from episodes where feed_id = ?" (Only fid)
    & fmap (fmap $ \((Only eid) :. ep) -> (eid, ep))

readFeed :: FeedId -> Connection -> IO (Maybe FeedInfo)
readFeed fid conn = do
    (fis :: [FeedInfo]) <- query conn "select name, url from feeds where id = ?" (Only fid)
    return $ listToMaybe $ fis

readPosition :: EpisodeId -> Connection -> IO ProgressMsg
readPosition eid conn = do
    (poss :: [ProgressMsg]) <- query conn "select episode_id, position, duration from progress where episode_id = ?" (Only eid)

    case poss of
        [] -> return $ ProgressMsg eid 0 0
        [pos] -> return pos
        _ -> fail $ "weird, position not unique for" ++ show eid

writeEpisodes :: Connection -> FeedId -> [Episode] -> IO ()
writeEpisodes conn fid eps =
    executeMany conn "insert or ignore into episodes(feed_id, url, title, date) values (?,?,?,?)"
            [ Only fid :. ep | ep <- eps ]

writeFeeds :: Connection -> [FeedInfo] -> IO ()
writeFeeds conn fis =
    executeMany conn "insert or ignore into feeds(name, url) values (?,?)" fis

writePosition :: ProgressMsg -> Connection -> IO ()
writePosition msg conn = do
    execute conn "insert or ignore into progress(episode_id, position, duration) values (?, ?, ?)" msg
    execute conn "update progress set position = ?, duration = ? where episode_id = ?" (proPos msg, prDuration msg, prEpId msg)
