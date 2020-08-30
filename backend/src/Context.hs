{-# LANGUAGE OverloadedStrings #-}

module Context where

import Data.Pool
import Database.PostgreSQL.Simple
import System.Log.Raven.Types (SentryService)

data Config = Config
  { dbString :: String,
    sentryDsn :: Maybe String,
    htmlPath :: FilePath,
    jsPath :: FilePath,
    restPort :: Int
  }
  deriving (Show)

data Context = Context
  { cConfig :: Config,
    cPool :: Pool Connection,
    sentry :: SentryService
  }
