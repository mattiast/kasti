{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Types where
import Data.Aeson
import Data.Aeson.Types(typeMismatch)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.String
import Data.Text(Text)
import Data.Time.Clock(UTCTime)
import Data.Pool

data Episode = Episode {
    epUrl :: EpisodeUrl
  , epTitle :: Text
  , epDate :: UTCTime
} deriving Show

newtype EpisodeUrl = EpisodeUrl {
    fromEpUrl :: Text
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
  , prDuration :: Double
} deriving Show

instance FromJSON ProgressMsg where
    parseJSON (Object v) = ProgressMsg
                            <$> v .: "episode_id"
                            <*> v .: "position"
                            <*> v .: "duration"
    parseJSON _ = mempty

instance ToJSON ProgressMsg where
    toJSON (ProgressMsg eid pos dur) =
        object ["episode_id" .= eid, "position" .= pos, "duration" .= dur]

-- items have state: New, Done, Not Started, In Progress (how much)
--
instance ToRow ProgressMsg where
    toRow (ProgressMsg eid pos dur) = toRow (eid, pos, dur)

instance FromRow ProgressMsg where
    fromRow = ProgressMsg <$> field <*> field <*> field

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
    toJSON fi = object ["name" .= fname fi, "url" .= furl fi]

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

data KastiConfig = KastiConfig {
    dbString :: String
  , staticPath :: FilePath
  , tlsCertPath :: FilePath
  , tlsKeyPath :: FilePath
} deriving Show

data KastiContext = KastiContext {
    cConfig :: KastiConfig
  , cPool :: Pool Connection
}

instance FromJSON KastiConfig where
    parseJSON (Object v) = KastiConfig
        <$> v .: "postgres_string"
        <*> v .: "static_path"
        <*> v .: "tls_cert_path"
        <*> v .: "tls_key_path"
    parseJSON _ = mempty
