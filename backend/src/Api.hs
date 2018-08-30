{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant.API
import Types

type ProgressApi =
  "progress" :>
    (Capture "episodeId" EpisodeId :> Get '[JSON] ProgressMsg :<|>
     "all" :> Get '[JSON] [ProgressInfo] :<|>
     ReqBody '[JSON] ProgressMsg :> Post '[JSON] ())

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
