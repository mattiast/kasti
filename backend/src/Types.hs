{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

module Types where
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.String
import Data.Text(Text)
import Data.Time.Clock(UTCTime)
import Elm
import GHC.Generics

data Episode = Episode {
    epUrl :: EpisodeUrl
  , epTitle :: Text
  , epDate :: UTCTime
} deriving (Show, Generic, ElmType, FromJSON, ToJSON)

newtype EpisodeUrl = EpisodeUrl {
    fromEpUrl :: Text
} deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (IsString, FromField, ToField)
  deriving anyclass (ElmType, FromJSON, ToJSON)

type EpisodeId = Int

data ProgressMsg = ProgressMsg {
    prEpId :: EpisodeId
  , prPos :: Double
  , prDuration :: Double
} deriving (Show, Generic, ElmType, FromJSON, ToJSON)

data FeedInfo = FeedInfo {
    fname :: Text
  , furl :: String
} deriving (Show, Generic, ElmType, FromJSON, ToJSON)

type FeedId = Int

data ProgressInfo = ProgressInfo 
    { ftitle :: String
    , epId :: EpisodeId
    , episode :: Episode
    , msg :: ProgressMsg
    }
    deriving (Show, Generic, ElmType, FromJSON, ToJSON)

instance ToRow Episode where
    toRow ep = toRow (epUrl ep, epTitle ep, epDate ep)

instance FromRow Episode where
    fromRow = Episode <$> field <*> field <*> field

instance ToRow ProgressMsg where
    toRow (ProgressMsg eid pos dur) = toRow (eid, pos, dur)

instance FromRow ProgressMsg where
    fromRow = ProgressMsg <$> field <*> field <*> field

instance ToRow FeedInfo where
    toRow fi = toRow (fname fi, furl fi)

instance FromRow FeedInfo where
    fromRow = FeedInfo <$> field <*> field
