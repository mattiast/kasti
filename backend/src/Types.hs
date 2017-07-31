{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Types where
import Data.Aeson
import Data.Aeson.Types(typeMismatch)
import Database.SQLite.Simple hiding ((:=))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.String
import Data.Text(Text)
import Data.Time.Clock(UTCTime)

data Episode = Episode {
    epUrl :: EpisodeUrl
  , epTitle :: Text
  , epDate :: UTCTime
} deriving Show

newtype EpisodeUrl = EpisodeUrl {
    fromEpUrl :: String
} deriving (Eq, Ord, Show, IsString, ToField, FromField, ToJSON)

newtype EpisodeId = EpisodeId { fromEpisodeId :: Int }
    deriving (Eq, Ord, Show, Num, FromField, ToField, ToJSON, FromJSON)

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
} deriving Show

instance FromJSON ProgressMsg where
    parseJSON (Object v) = ProgressMsg
                            <$> v .: "episode_id"
                            <*> v .: "position"
    parseJSON _ = mempty
-- items have state: New, Done, Not Started, In Progress (how much)
--
instance ToRow ProgressMsg where
    toRow msg = toRow (prEpId msg, proPos msg)

instance FromRow ProgressMsg where
    fromRow = ProgressMsg <$> field <*> field

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

data UserInfo = UserInfo {
    userGithubId :: Int
  , userName :: Text
  , userGithubLogin :: Text
} deriving Show

instance FromJSON UserInfo where
    parseJSON (Object v)
        = UserInfo
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "login"
    parseJSON _ = mempty
