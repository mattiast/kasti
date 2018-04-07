{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module PodEff
    ( PodEff(..)
    , getFeedInfo
    , getFeeds
    , runPod
    ) where
import Types
import Control.Eff
import Control.Eff.Lift
import Database.PostgreSQL.Simple(Connection)
import EpisodeDb

data PodEff a where
    PodGetFeedInfo :: FeedId -> PodEff (Maybe FeedInfo)
    PodGetFeeds :: PodEff [(FeedId, FeedInfo)]

getFeedInfo :: (Member PodEff e) => FeedId -> Eff e (Maybe FeedInfo)
getFeedInfo fid = send (PodGetFeedInfo fid)

getFeeds :: (Member PodEff e) => Eff e [(FeedId, FeedInfo)]
getFeeds = send PodGetFeeds

runPod :: (SetMember Lift (Lift IO) r) => Connection -> Eff (PodEff ': r) w -> Eff r w
runPod conn = handle_relay return (oneQ conn)

oneQ :: (SetMember Lift (Lift IO) r) => Connection -> PodEff v -> Arr r v a -> Eff r a
oneQ conn x f = lift (query x conn) >>= f

query :: PodEff a -> Connection -> IO a
query x = case x of
    PodGetFeeds -> readFeeds
    PodGetFeedInfo fid -> readFeed fid
