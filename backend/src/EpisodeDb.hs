{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module EpisodeDb where
import Control.Monad(void)
import Database.PostgreSQL.Simple
import Data.Function((&))
import Data.Maybe(listToMaybe)
import Types
import Data.Pool
import Data.Time.Clock(NominalDiffTime)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

initPool :: String -> IO (Pool Connection)
initPool connString = do
    putStrLn "Initiating DB connection pool"
    createPool
        (connectPostgreSQL $ B.pack connString)
        (close :: Connection -> IO ())
        1  -- num of stripes in the pool
        (30 :: NominalDiffTime)
        5  -- num of connections per stripe

closePool :: Pool Connection -> IO ()
closePool pool = do
    destroyAllResources pool
    putStrLn "DB connection pool closed"

readFeeds :: Connection -> IO [FStuff]
readFeeds conn = do
    rows <- query_ conn "select id, name, url from feeds"
    return [ FStuff fid fi | Only fid :. fi <- rows ]

readEpisodes :: FeedId -> Connection -> IO [EStuff]
readEpisodes fid conn = do
    rows <- query conn "select id, url, title, date from episodes where feed_id = ?" (Only fid)
    return [ EStuff eid ep | Only eid :. ep <- rows ]

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

readPositions :: Connection -> IO [ProgressInfo]
readPositions conn = do
    (rows :: [(String, EpisodeId) :. Episode :. ProgressMsg]) <- query_ conn
      "select f.name, e.id, e.url, e.title, e.date, p.episode_id, p.position, p.duration \
      \ from progress as p \
      \ join episodes as e on p.episode_id = e.id \
      \ join feeds as f on e.feed_id = f.id \
      \ where p.position < p.duration"
    return [ProgressInfo ftitle eid ep msg | (ftitle, eid) :. ep :. msg <- rows ]

readNewEpisodes :: Int -> Connection -> IO [NewEpisode]
readNewEpisodes n conn = do
    (rows :: [(String, EpisodeId) :. Episode]) <- query conn
        "select f.name, e.id, e.url, e.title, e.date \
        \ from (select * \
               \ from episodes \
               \ order by id desc \
               \ limit ?) as e \
        \ join feeds as f on e.feed_id = f.id \
        \ order by e.id desc" (Only n)
    return [NewEpisode ftitle eid ep | (ftitle, eid) :. ep <- rows ]


writeEpisodes :: FeedId -> [Episode] -> Connection -> IO Int
writeEpisodes fid eps conn = withTransaction conn $ do
    (newUrls :: [EpisodeUrl]) <-
        fmap (fmap fromOnly) $ query conn "select url from unnest (?) as t(url) where url not in (select url from episodes)" (Only $ V.fromList $ fmap epUrl eps)
    let newEpisodes = filter (\e -> epUrl e `elem` newUrls) eps
    numRows <- executeMany conn "insert into episodes(feed_id, url, title, date) values (?,?,?,?) on conflict do nothing"
            [ Only fid :. ep | ep <- newEpisodes ]
    return $! fromIntegral numRows

writeFeed :: FeedInfo -> Connection -> IO ()
writeFeed fi conn =
    execute conn "insert into feeds(name, url) values (?,?) on conflict do nothing" fi
    & withTransaction conn
    & void

writePosition :: ProgressMsg -> Connection -> IO ()
writePosition msg conn = withTransaction conn $ void $
    execute conn "insert into progress(episode_id, position, duration) values (?, ?, ?)\
        \ on conflict(episode_id) do update set position = ?, duration = ?" (msg :. (prPos msg, prDuration msg))
