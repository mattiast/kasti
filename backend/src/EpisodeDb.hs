{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module EpisodeDb where
import Control.Monad(void)
import Database.PostgreSQL.Simple
import Data.Function((&))
import Data.Maybe(listToMaybe)
import Types
import Data.Pool
import qualified Data.ByteString.Char8 as B

initPool :: String -> IO (Pool Connection)
initPool connString =
    createPool (connectPostgreSQL $ B.pack connString) close 30 1 10

readFeeds :: Connection -> IO [(FeedId, FeedInfo)]
readFeeds conn = query_ conn "select id, name, url from feeds"
    & fmap (fmap $ \(Only fid :. fi) -> (fid, fi))

readEpisodes :: FeedId -> Connection -> IO [(EpisodeId, Episode)]
readEpisodes fid conn = query conn "select id, url, title, date from episodes where feed_id = ?" (Only fid)
    & fmap (fmap $ \(Only eid :. ep) -> (eid, ep))

readFeed :: FeedId -> Connection -> IO (Maybe FeedInfo)
readFeed fid conn = do
    (fis :: [FeedInfo]) <- query conn "select name, url from feeds where id = ?" (Only fid)
    return $ listToMaybe fis

readPosition :: EpisodeId -> Connection -> IO ProgressMsg
readPosition eid conn = do
    (poss :: [ProgressMsg]) <- query conn "select episode_id, position, duration from progress where episode_id = ?" (Only eid)

    case poss of
        [] -> return $ ProgressMsg eid 0 0
        [pos] -> return pos
        _ -> fail $ "weird, position not unique for" ++ show eid

readPositions :: Connection -> IO [(String, (EpisodeId, Episode), ProgressMsg)]
readPositions conn = do
    (rows :: [(String, EpisodeId) :. Episode :. ProgressMsg]) <- query_ conn
      "select f.name, e.id, e.url, e.title, e.date, p.episode_id, p.position, p.duration \
      \ from progress as p \
      \ join episodes as e on p.episode_id = e.id \
      \ join feeds as f on e.feed_id = f.id \
      \ where p.position < p.duration"
    return [(ftitle, (eid, ep), msg) | (ftitle, eid) :. ep :. msg <- rows ]

readNewEpisodes :: Int -> Connection -> IO [(String, (EpisodeId, Episode))]
readNewEpisodes n conn = do
    (rows :: [(String, EpisodeId) :. Episode]) <- query conn
        "select f.name, e.id, e.url, e.title, e.date \
        \ from (select * \
               \ from episodes \
               \ order by id desc \
               \ limit ?) as e \
        \ join feeds as f on e.feed_id = f.id" (Only n)
    return [(ftitle, (eid, ep)) | (ftitle, eid) :. ep <- rows ]


writeEpisodes :: Connection -> FeedId -> [Episode] -> IO ()
writeEpisodes conn fid eps =
    executeMany conn "insert into episodes(feed_id, url, title, date) values (?,?,?,?) on conflict do nothing"
            [ Only fid :. ep | ep <- eps ]
    & withTransaction conn
    & void

writeFeeds :: Connection -> [FeedInfo] -> IO ()
writeFeeds conn fis =
    executeMany conn "insert into feeds(name, url) values (?,?) on conflict do nothing" fis
    & withTransaction conn
    & void

writePosition :: ProgressMsg -> Connection -> IO ()
writePosition msg conn = withTransaction conn $ do
    execute conn "insert into progress(episode_id, position, duration) values (?, ?, ?)\
        \ on conflict(episode_id) do update set position = ?, duration = ?" (msg :. (proPos msg, prDuration msg))
    return ()
