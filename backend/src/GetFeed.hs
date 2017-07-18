{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module GetFeed where
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString.Lazy(ByteString)
import Data.Function((&))
import Data.Maybe
import Data.Text(pack)
import Data.Time.Clock(UTCTime)
import Data.Time.Format
import Data.Foldable(traverse_)
import Network.Wreq hiding ((:=))
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax(DateString)
import Types
import EpisodeDb

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

syncFeed :: FeedId -> IO ()
syncFeed fid = withConn $ \conn -> do
    fi <- getFeedInfo fid conn
    eps <- fetchEpisodes fi
    writeEpisodes conn fid eps

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
        feedsFromDB conn >>= (traverse_ $ \(fid, fi) -> do
            eps <- fetchEpisodes fi
            writeEpisodes conn fid eps)
