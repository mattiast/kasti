{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module GetFeed where
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Function((&))
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock(UTCTime)
import Data.Time.Format
import Database.PostgreSQL.Simple(Connection)
import Network.Wreq hiding ((:=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax(DateString)
import Types
import EpisodeDb
import Data.Pool
import Control.Concurrent.Async(forConcurrently_)

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    r <- get url
    let body = r ^. responseBody
    return $ parseFeedSource body

type ContIO a = forall b . (a -> IO b) -> IO b

syncFeeds :: (Foldable t) => t (FeedId, FeedInfo) -> ContIO Connection -> IO ()
syncFeeds fs conn = do
    let parallelDownloads = 5
    dlPool <- createPool (return ()) (\() -> return ()) 1 1.234 parallelDownloads
    forConcurrently_ fs $ \(fid, fi) -> do
        es <- withResource dlPool $ const $ fetchEpisodes fi
        conn $ writeEpisodes fid es

fetchEpisodes :: FeedInfo -> IO [Episode]
fetchEpisodes fi = do
    feed <- fetchFeed (furl fi)
    let items = maybe [] feedItems feed
    return $ mapMaybe itemEpisodeInfo items

parsePubDate :: DateString -> Maybe UTCTime
parsePubDate = parseTimeM True defaultTimeLocale rfc822DateFormat . T.unpack

itemEpisodeInfo :: Item -> Maybe Episode
itemEpisodeInfo item = do
    title <- getItemTitle item
    (url, _, _) <- getItemEnclosure item
    date <- getItemDate item
        >>= parsePubDate
    return $ Episode
        (EpisodeUrl url)
        title
        date
