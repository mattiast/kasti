{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module GetFeed where
import Control.Lens
import Control.Monad((>=>),(<=<))
import Data.Aeson
import Data.Aeson.Types(typeMismatch)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import Data.ByteString.Lazy(ByteString)
import Data.Function((&))
import Data.Maybe
import Data.String
import Data.Text(Text, pack)
import Data.Time.Calendar
import Data.Time.Clock(UTCTime)
import Data.Time.Format
import Data.Traversable(forM)
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax(DateString)

type Name = Text
type Url = String

data FeedInfo = FeedInfo {
    fname :: Text
  , furl :: String
}

newtype FeedId = FeedId { fromFeedId :: Int }
    deriving (Eq, Ord, Num, FromField, ToField)

instance FromJSON FeedInfo where
    parseJSON (Object v) = FeedInfo <$> v .: "name" <*> v .: "url"
    parseJSON wat = typeMismatch "FeedInfo" wat

instance ToRow FeedInfo where
    toRow fi = toRow (fname fi, furl fi)

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
    [[feed_id :: FeedId]] <- query conn "select id from feeds where url = ?" (Only (furl fi))
    (_, eps) <- fetchEpisodes fi
    executeMany conn "insert into episodes(feed_id, url, title, date) values (?,?,?,?)"
            [ Only feed_id :. ep | ep <- eps ]
    return ()

data Episode = Episode {
    epUrl :: EpisodeUrl
  , epTitle :: Text
  , epDate :: UTCTime
} deriving Show

newtype EpisodeUrl = EpisodeUrl {
    fromEpUrl :: String
} deriving (Eq, Ord, Show, IsString, ToField)

instance ToRow Episode where
    toRow ep = toRow (epUrl ep, epTitle ep, epDate ep)
-- items have state: New, Done, Not Started, In Progress (how much)

fetchEpisodes :: FeedInfo -> IO (String, [Episode])
fetchEpisodes fi = do
    let url = furl fi
    Just feed <- fetchFeed url
    let title = getFeedTitle feed
        items = feedItems feed
    return (title, mapMaybe itemEpisodeInfo items)

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
    conn <- open "db.sqlite" 
    writeFeeds conn fs
    sequence_ [ writeEpisodes conn fi | fi <- fs ]
