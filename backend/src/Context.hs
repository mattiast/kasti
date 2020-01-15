{-# LANGUAGE OverloadedStrings #-}
module Context where
import Data.Pool
import Database.PostgreSQL.Simple

data Config = Config {
    dbString :: String
  , htmlPath :: FilePath
  , jsPath :: FilePath
  , restPort :: Int
} deriving Show

data Context = Context {
    cConfig :: Config
  , cPool :: Pool Connection
}
