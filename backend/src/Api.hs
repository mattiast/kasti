{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Elm      (ElmType)
import Types

type Api =
  "progress" :>
    (Capture "episodeId" EpisodeId :> Get '[JSON] ProgressMsg :<|>
     "all" :> Get '[JSON] [ProgressInfo] :<|>
     ReqBody '[JSON] ProgressMsg :> Post '[JSON] ()) :<|>
  "feeds" :> Get '[JSON] [(FeedId, FeedInfo)]
