{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant.API
import Types

type Api =
  "progress" :>
    (Capture "episodeId" EpisodeId :> Get '[JSON] ProgressMsg :<|>
     "all" :> Get '[JSON] [ProgressInfo] :<|>
     ReqBody '[JSON] ProgressMsg :> Post '[JSON] ()) :<|>
  "feeds" :> Get '[JSON] [(FeedId, FeedInfo)] :<|>
  "feed" :> ReqBody '[JSON] FeedInfo :> Post '[JSON] () :<|>
  "episodes" :> "new" :> Get '[JSON] [NewEpisode]
