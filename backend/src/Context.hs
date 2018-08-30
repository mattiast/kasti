{-# LANGUAGE OverloadedStrings #-}
module Context where
import Data.Pool
import Database.PostgreSQL.Simple
import Data.Aeson

data Config = Config {
    dbString :: String
  , staticPath :: FilePath
} deriving Show

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: "postgres_string"
        <*> v .: "static_path"
    parseJSON _ = mempty

data Context = Context {
    cConfig :: Config
  , cPool :: Pool Connection
}
