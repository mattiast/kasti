{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api where

import Servant.API
import Types
import GHC.Generics
import Elm
import Data.Aeson

data OK = OK { okText :: String }
    deriving (Generic, ElmType, FromJSON, ToJSON)

type ProgressApi =
  "progress" :>
    (Capture "episodeId" EpisodeId :> Get '[JSON] ProgressMsg :<|>
     "all" :> Get '[JSON] [ProgressInfo] :<|>
     ReqBody '[JSON] ProgressMsg :> Post '[JSON] OK)

type EpisodeApi =
  "episodes" :>
    ("new" :> Get '[JSON] [NewEpisode] :<|>
     Capture "feedId" FeedId :> Get '[JSON] [(EpisodeId, Episode)])

type SyncFeedApi = 
  "syncfeed" :>
    ("all" :> Post '[JSON] () :<|>
     Capture "feedId" FeedId :> Post '[JSON] ())

type FeedsApi = "feeds" :> Get '[JSON] [(FeedId, FeedInfo)]
type FeedApi = "feed" :> ReqBody '[JSON] FeedInfo :> Post '[JSON] ()

type Api =
  ProgressApi :<|>
  FeedsApi :<|>
  FeedApi :<|>
  EpisodeApi :<|>
  SyncFeedApi
