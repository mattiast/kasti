{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}

module PodEff where
import Types
import Control.Eff

data PodEff a where
    PodRead :: EpisodeId -> PodEff Episode

getEpi :: (Member PodEff e) => EpisodeId -> Eff e Episode
getEpi eid = send (PodRead eid)
