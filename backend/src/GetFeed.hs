{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module GetFeed
  ( syncFeeds
  ) where

import Control.Applicative
import Control.Concurrent.Async.Lifted.Safe (forConcurrently_)
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Database.PostgreSQL.Simple (Connection)
import EpisodeDb
import Network.Wreq hiding ((:=))
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax (DateString)
import Types

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    r <- get url
    let body = r ^. responseBody
    return $ parseFeedSource body

syncFeeds :: (Foldable t) => t (FeedId, FeedInfo) -> ReaderT Connection IO ()
syncFeeds fs = do
    let parallelDownloads = 5
    dlPool <- liftIO $ createPool (return ()) (\() -> return ()) 1 1.234 parallelDownloads
    forConcurrently_ fs $ \(fid, fi) -> do
        es <- liftIO $ withResource dlPool $ const $ fetchEpisodes fi
        conn <- ask
        liftIO $ writeEpisodes fid es conn

fetchEpisodes :: FeedInfo -> IO [Episode]
fetchEpisodes fi = do
    feed <- fetchFeed (furl fi)
    let items = maybe [] feedItems feed
    return $ mapMaybe itemEpisodeInfo items

parsePubDate :: DateString -> Maybe UTCTime
parsePubDate dt = let ds = T.unpack dt
                   in (parseTimeM True defaultTimeLocale rfc822DateFormat ds)
                   <|> (parseTimeM True defaultTimeLocale "%A, %e %B %Y %k:%M:%S %Z" ds)

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
