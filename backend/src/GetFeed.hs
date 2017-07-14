{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module GetFeed where
import Control.Lens hiding ((.=))
import Control.Monad((>=>),(<=<))
import Data.Aeson
import Data.Aeson.Types(typeMismatch)
import Database.SQLite.Simple hiding ((:=))
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
import Network.Wreq hiding ((:=))
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
    deriving (Eq, Ord, Num, FromField, ToField, ToJSON)

instance FromJSON FeedInfo where
    parseJSON (Object v) = FeedInfo <$> v .: "name" <*> v .: "url"
    parseJSON wat = typeMismatch "FeedInfo" wat

instance ToJSON FeedInfo where
    toJSON (FeedInfo name url) =
        object ["name" .= name, "url" .= url]

instance ToRow FeedInfo where
    toRow fi = toRow (fname fi, furl fi)

instance FromRow FeedInfo where
    fromRow = FeedInfo <$> field <*> field

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
} deriving (Eq, Ord, Show, IsString, ToField, FromField, ToJSON)

newtype EpisodeId = EpisodeId { fromEpisodeId :: Int }
    deriving (Eq, Ord, Num, FromField, ToField, ToJSON, FromJSON)

instance ToRow Episode where
    toRow ep = toRow (epUrl ep, epTitle ep, epDate ep)

instance FromRow Episode where
    fromRow = Episode <$> field <*> field <*> field

instance ToJSON Episode where
    toJSON ep =
        object ["url" .= epUrl ep, "title" .= epTitle ep, "date" .= epDate ep]


data ProgressMsg = ProgressMsg {
    prEpId :: EpisodeId
  , proPos :: Double
}

instance FromJSON ProgressMsg where
    parseJSON (Object v) = ProgressMsg
                            <$> v .: "episode_id"
                            <*> v .: "position"
    parseJSON _ = mempty
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

feedsFromDB :: Connection -> IO [(FeedId, FeedInfo)]
feedsFromDB conn = query_ conn "select id, name, url from feeds"
    & fmap (fmap $ \((Only id) :. fi) -> (id, fi))

episodesFromDB :: FeedId -> Connection -> IO [Episode]
episodesFromDB fid conn = query conn "select url, title, date from episodes where feed_id = ?" (Only fid)

withConn :: (Connection -> IO a) -> IO a
withConn = withConnection "db.sqlite"

savePosition :: ProgressMsg -> Connection -> IO ()
savePosition msg conn = do
    return ()

getPosition :: EpisodeId -> Connection -> IO Double
getPosition eid conn = do
    return 15
