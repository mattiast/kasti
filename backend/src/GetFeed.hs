{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module GetFeed where
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Function((&))
import Data.Maybe
import Data.Foldable(traverse_)
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

getFeeds :: FilePath -> IO [FeedInfo]
getFeeds path = do
    (bs :: BL.ByteString) <- BL.readFile path
    return $ decode bs
        & fromMaybe []

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    r <- get url
    let body = r ^. responseBody
    return $ parseFeedSource body

type ContIO a = forall b . (a -> IO b) -> IO b

syncFeed :: FeedId -> ContIO Connection -> IO ()
syncFeed fid conn = do
    x <- conn $ readFeed fid
    esm <- mapM fetchEpisodes x
    traverse_ (\es -> conn $ writeEpisodes fid es) esm

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
