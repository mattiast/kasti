{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

import Servant.API
import Servant.Elm
import Types

type NoCache a = Headers '[Header "Cache-Control" String] a

noCache :: a -> NoCache a
noCache = addHeader "no-cache, no-store, must-revalidate"

type ProgressApi =
  "progress" :>
    (Capture "episodeId" Int :> Get '[JSON] (NoCache ProgressMsg) :<|>
     "all" :> Get '[JSON] (NoCache [ProgressInfo]) :<|>
     ReqBody '[JSON] ProgressMsg :> Post '[JSON] ())

type EpisodeApi =
  "episodes" :>
    ("new" :> Get '[JSON] (NoCache [NewEpisode]) :<|>
     Capture "feedId" FeedId :> Get '[JSON] (NoCache [EStuff]))


type SyncFeedApi = 
  "syncfeed" :>
    ("all" :> Post '[JSON] () :<|>
     Capture "feedId" FeedId :> Post '[JSON] ())

type FeedsApi = "feeds" :> Get '[JSON] [FStuff]
type FeedApi = "feed" :> ReqBody '[JSON] FeedInfo :> Post '[JSON] ()

type Api =
  ProgressApi :<|>
  FeedsApi :<|>
  FeedApi :<|>
  EpisodeApi :<|>
  SyncFeedApi
