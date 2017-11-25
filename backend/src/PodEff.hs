{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module PodEff where
import Types
import Control.Eff
import Database.PostgreSQL.Simple(Connection)
import EpisodeDb

data PodEff a where
    PodGetFeedInfo :: FeedId -> PodEff (Maybe FeedInfo)
    PodGetFeeds :: PodEff [(FeedId, FeedInfo)]

getFeedInfo :: (Member PodEff e) => FeedId -> Eff e (Maybe FeedInfo)
getFeedInfo fid = send (PodGetFeedInfo fid)

getFeeds :: (Member PodEff e) => Eff e [(FeedId, FeedInfo)]
getFeeds = send PodGetFeeds

runPod :: (Member IO r) => Connection -> Eff (PodEff ': r) w -> Eff r w
runPod conn s = handle_relay return (\(x :: PodEff v) (f :: Arr r v w) -> oneQ conn x f) s

oneQ :: (Member IO r) => Connection -> PodEff v -> Arr r v a -> Eff r a
oneQ conn (PodGetFeedInfo fid) f = send (readFeed fid conn) >>= f
oneQ conn PodGetFeeds f = send (readFeeds conn) >>= f
