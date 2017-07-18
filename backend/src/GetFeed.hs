{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module GetFeed where
import Control.Lens hiding ((.=))
import Data.Aeson
import Database.SQLite.Simple hiding ((:=))
import Data.ByteString.Lazy(ByteString)
import Data.Function((&))
import Data.Maybe
import Data.Text(pack)
import Data.Time.Clock(UTCTime)
import Data.Time.Format
import Network.Wreq hiding ((:=))
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax(DateString)
import Types
import EpisodeDb(withConn)

getFeeds :: FilePath -> IO [FeedInfo]
getFeeds path = do
    (bs :: ByteString) <- B.readFile path
    return $ decode bs
        & fromMaybe []

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    r <- get url
    let b = r ^. responseBody
        feed = parseFeedString $ B.unpack $ b
    return feed

writeFeeds :: Connection -> [FeedInfo] -> IO ()
writeFeeds conn fis = do
    executeMany conn "insert into feeds(name, url) values (?,?)" fis

writeEpisodes :: Connection -> FeedInfo -> IO ()
writeEpisodes conn fi = do
    -- TODO this is bad, fix the assumption
    [[feed_id :: FeedId]] <- query conn "select id from feeds where url = ?" (Only (furl fi))
    eps <- fetchEpisodes fi
    executeMany conn "insert or ignore into episodes(feed_id, url, title, date) values (?,?,?,?)"
            [ Only feed_id :. ep | ep <- eps ]
    return ()

fetchEpisodes :: FeedInfo -> IO [Episode]
fetchEpisodes fi = do
    Just feed <- fetchFeed (furl fi)
    let items = feedItems feed
    return $ mapMaybe itemEpisodeInfo items

parsePubDate :: DateString -> Maybe UTCTime
parsePubDate = parseTimeM True defaultTimeLocale rfc822DateFormat

itemEpisodeInfo :: Item -> Maybe Episode
itemEpisodeInfo item = do
    title <- getItemTitle item
    (url, _, _) <- getItemEnclosure item
    date <- getItemDate item
        >>= parsePubDate
    return $ Episode
        (EpisodeUrl url)
        (pack title)
        date

populateDB :: IO ()
populateDB = do
    fs <- getFeeds "/home/matti/.vim/podcasts.json" 
    withConn $ \conn -> do
        writeFeeds conn fs
        sequence_ [ writeEpisodes conn fi | fi <- fs ]
