{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module GetFeed where
import Control.Lens hiding ((.=))
import Control.Monad(void)
import Data.Aeson
import Data.Function((&))
import Data.Maybe
import qualified Data.Text.Encoding as T
import Data.Time.Clock(UTCTime)
import Data.Time.Format
import Data.Foldable(traverse_)
import Database.SQLite.Simple(Connection, withConnection)
import Network.Wreq hiding ((:=))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax(DateString)
import Types
import EpisodeDb

getFeeds :: FilePath -> IO [FeedInfo]
getFeeds path = do
    (bs :: BL.ByteString) <- BL.readFile path
    return $ decode bs
        & fromMaybe []

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    r <- get url
    let b = r ^. responseBody
        feed = parseFeedString $ BL.unpack $ b
    return feed

syncFeed :: FeedId -> Connection -> IO ()
syncFeed fid conn =
    readFeed fid conn
    >>= mapM fetchEpisodes
    >>= mapM (writeEpisodes conn fid)
    & void

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
        (T.decodeUtf8 $ B.pack title)
        date

populateDB :: IO ()
populateDB = do
    fs <- getFeeds "/home/matti/.vim/podcasts.json" 
    withConnection "db.sqlite" $ \conn -> do
        writeFeeds conn fs
        readFeeds conn >>= (traverse_ $ \(fid, fi) -> do
            eps <- fetchEpisodes fi
            writeEpisodes conn fid eps)
