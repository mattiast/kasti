{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module GetFeed
  ( syncFeeds
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Maybe
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
import UnliftIO.Exception(tryAny)
import UnliftIO.Async(pooledForConcurrentlyN)
import Data.Either(isRight)
import Data.Foldable(traverse_)

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    r <- get url
    let body = r ^. responseBody
    return $ parseFeedSource body

syncFeeds :: (Traversable t) => t (FeedId, FeedInfo) -> ReaderT Connection IO ()
syncFeeds fs = do
    let parallelDownloads = 5
    allEpis <- pooledForConcurrentlyN parallelDownloads fs $ \(fid, fi) -> tryAny $ do
        es <- liftIO $ fetchEpisodes fi
        return (fid, es)

    let successfulEpis = sum $ fmap (\x -> if isRight x then 1 else 0) allEpis :: Int
    liftIO $ putStrLn $ "Got " ++ show (successfulEpis) ++ " out of " ++ show (length fs)
    conn <- ask
    forM_ allEpis (traverse_ (\(fid, es) -> liftIO (writeEpisodes fid es conn)))

fetchEpisodes :: FeedInfo -> IO [Episode]
fetchEpisodes fi = do
    feed <- fetchFeed (furl fi)
    let items = maybe [] feedItems feed
    return $ mapMaybe itemEpisodeInfo items

parsePubDate :: DateString -> Maybe UTCTime
parsePubDate dt = let ds = T.unpack dt
                   in (parseTimeM True defaultTimeLocale rfc822DateFormat ds)
                   <|> (parseTimeM True defaultTimeLocale "%A, %e %B %Y %k:%M:%S %Z" ds)
                   <|> (parseTimeM True defaultTimeLocale "%a, %e %b %Y %k:%M:%S" ds)  -- "Thu, 13 Feb 2020 00:00:00"

itemEpisodeInfo :: Item -> Maybe Episode
itemEpisodeInfo item = do
    title <- getItemTitle item
    (url, _, _) <- getItemEnclosure item
    date <- getItemDate item
        >>= parsePubDate
    return $ Episode
        url
        title
        date
