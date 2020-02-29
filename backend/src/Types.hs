{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where
import Data.Aeson hiding (defaultOptions)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.String
import Data.Text(Text)
import Data.Time.Clock(UTCTime)
import GHC.Generics
import Elm.Derive


data Episode = Episode {
    epUrl :: EpisodeUrl
  , epTitle :: Text
  , epDate :: UTCTime
} deriving (Show, Generic)


type EpisodeUrl = Text

type EpisodeId = Int

data ProgressMsg = ProgressMsg {
    prEpId :: EpisodeId
  , prPos :: Double
  , prDuration :: Double
} deriving (Show, Generic)


data FeedInfo = FeedInfo {
    fname :: Text
  , furl :: String
} deriving (Show, Generic)


type FeedId = Int

data EStuff = EStuff EpisodeId Episode
data FStuff = FStuff FeedId FeedInfo

deriveBoth defaultOptions ''EStuff
deriveBoth defaultOptions ''FStuff

data ProgressInfo = ProgressInfo 
    { pi_ftitle :: String
    , pi_epId :: EpisodeId
    , pi_episode :: Episode
    , pi_prog :: ProgressMsg
    }
    deriving (Show, Generic)


data NewEpisode = NewEpisode 
    { ne_ftitle :: String
    , ne_epId :: EpisodeId
    , ne_episode :: Episode
    }
    deriving (Show, Generic)

deriveBoth defaultOptions ''Episode
deriveBoth defaultOptions ''ProgressMsg
deriveBoth defaultOptions ''FeedInfo
deriveBoth defaultOptions ''ProgressInfo
deriveBoth defaultOptions ''NewEpisode

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
