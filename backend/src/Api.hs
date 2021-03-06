{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant.API
import Types

type NoCache a = Headers '[Header "Cache-Control" String] a

noCache :: a -> NoCache a
noCache = addHeader "no-cache, no-store, must-revalidate"

type ProgressApi =
  "progress"
    :> ( Capture "episodeId" Int :> Get '[JSON] (NoCache ProgressMsg)
           :<|> "all" :> Get '[JSON] (NoCache [ProgressInfo])
           :<|> ReqBody '[JSON] ProgressMsg :> Post '[JSON] ()
       )

type EpisodeApi =
  "episodes"
    :> ( "new" :> Get '[JSON] (NoCache [NewEpisode])
           :<|> Capture "feedId" FeedId :> Get '[JSON] (NoCache [EStuff])
       )

type SyncFeedApi =
  "syncfeed"
    :> ( "all" :> Post '[JSON] Int
           :<|> Capture "feedId" FeedId :> Post '[JSON] Int
       )

type FeedsApi = "feeds" :> Get '[JSON] [FStuff]

type FeedApi = "feed" :> ReqBody '[JSON] FeedInfo :> Post '[JSON] ()

type Api =
  ProgressApi
    :<|> FeedsApi
    :<|> FeedApi
    :<|> EpisodeApi
    :<|> SyncFeedApi
