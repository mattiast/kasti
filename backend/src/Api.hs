{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api where

import Servant.API
import Types

type ProgressApi =
  "progress" :>
    (Capture "episodeId" EpisodeId :> Get '[JSON] ProgressMsg :<|>
     "all" :> Get '[JSON] [ProgressInfo] :<|>
     ReqBody '[JSON] ProgressMsg :> Post '[JSON] NoContent)

type EpisodeApi =
  "episodes" :>
    ("new" :> Get '[JSON] [NewEpisode] :<|>
     Capture "feedId" FeedId :> Get '[JSON] [(EpisodeId, Episode)])

type SyncFeedApi = 
  "syncfeed" :>
    ("all" :> Post '[JSON] NoContent :<|>
     Capture "feedId" FeedId :> Post '[JSON] NoContent)

type FeedsApi = "feeds" :> Get '[JSON] [(FeedId, FeedInfo)]
type FeedApi = "feed" :> ReqBody '[JSON] FeedInfo :> Post '[JSON] NoContent

type Api =
  ProgressApi :<|>
  FeedsApi :<|>
  FeedApi :<|>
  EpisodeApi :<|>
  SyncFeedApi
