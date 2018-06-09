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
    , getPositions
    , getPosition
    , getEpisodes
    , getNewEpisodes
    , saveFeedInfo
    , saveProgress
    , runPod
    ) where
import Types
import Control.Monad.Freer
import Database.PostgreSQL.Simple(Connection)
import EpisodeDb

data PodEff a where
    PodGetFeedInfo :: FeedId -> PodEff (Maybe FeedInfo)
    PodGetFeeds :: PodEff [(FeedId, FeedInfo)]
    PodGetProgressAll :: PodEff [ProgressInfo]
    PodGetProgress :: EpisodeId -> PodEff ProgressMsg
    PodGetFeedEpisodes :: FeedId -> PodEff [(EpisodeId, Episode)]
    PodGetNewEpisodes :: Int -> PodEff [NewEpisode]
    PodWriteFeed :: FeedInfo -> PodEff ()
    PodWriteProgress :: ProgressMsg -> PodEff ()

getFeedInfo :: (Member PodEff e) => FeedId -> Eff e (Maybe FeedInfo)
getFeedInfo fid = send (PodGetFeedInfo fid)

getFeeds :: (Member PodEff e) => Eff e [(FeedId, FeedInfo)]
getFeeds = send PodGetFeeds

getPositions :: (Member PodEff e) => Eff e [ProgressInfo]
getPositions = send PodGetProgressAll

getPosition :: (Member PodEff e) => EpisodeId -> Eff e ProgressMsg
getPosition eid = send (PodGetProgress eid)

getNewEpisodes :: (Member PodEff e) => Int -> Eff e [NewEpisode]
getNewEpisodes n = send (PodGetNewEpisodes n)

getEpisodes ::  (Member PodEff e) => FeedId -> Eff e [(EpisodeId, Episode)]
getEpisodes fid = send (PodGetFeedEpisodes fid)

saveFeedInfo :: (Member PodEff e) => FeedInfo -> Eff e ()
saveFeedInfo fi = send (PodWriteFeed fi)

saveProgress :: (Member PodEff e) => ProgressMsg -> Eff e ()
saveProgress prog = send (PodWriteProgress prog)

runPod :: (LastMember IO r) => Connection -> Eff (PodEff ': r) w -> Eff r w
runPod conn = interpretM (flip query conn)

query :: PodEff a -> Connection -> IO a
query x = case x of
    PodGetFeeds -> readFeeds
    PodGetFeedInfo fid -> readFeed fid
    PodGetProgressAll -> readPositions
    PodGetProgress eid -> readPosition eid
    PodGetFeedEpisodes fid -> readEpisodes fid
    PodGetNewEpisodes n -> readNewEpisodes n
    PodWriteFeed fi -> writeFeed fi
    PodWriteProgress prog -> writePosition prog
