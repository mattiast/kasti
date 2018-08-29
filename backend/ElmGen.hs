{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Proxy
import Types
import Api
import Elm
import Servant.Elm

spec :: Spec
spec =
  Spec
    ["Client", "Types"]
    ([ "import Json.Decode exposing (..)"
    , "import Json.Decode.Pipeline exposing (..)"
    , "import Exts.Json.Decode exposing (..)"
    , "import Date exposing (Date)"
    , "import Json.Encode"
    , "import Http"

    , toElmTypeSource (Proxy :: Proxy Episode)
    , toElmDecoderSource (Proxy :: Proxy Episode)
    , toElmEncoderSource (Proxy :: Proxy Episode)

    , toElmTypeSource (Proxy :: Proxy EpisodeUrl)
    , toElmDecoderSource (Proxy :: Proxy EpisodeUrl)
    , toElmEncoderSource (Proxy :: Proxy EpisodeUrl)

    , toElmTypeSource (Proxy :: Proxy ProgressMsg)
    , toElmDecoderSource (Proxy :: Proxy ProgressMsg)
    , toElmEncoderSource (Proxy :: Proxy ProgressMsg)

    , toElmTypeSource (Proxy :: Proxy FeedInfo)
    , toElmDecoderSource (Proxy :: Proxy FeedInfo)
    , toElmEncoderSource (Proxy :: Proxy FeedInfo)

    , toElmTypeSource (Proxy :: Proxy ProgressInfo)
    , toElmDecoderSource (Proxy :: Proxy ProgressInfo)
    , toElmEncoderSource (Proxy :: Proxy ProgressInfo)

    , toElmTypeSource (Proxy :: Proxy NewEpisode)
    , toElmDecoderSource (Proxy :: Proxy NewEpisode)
    , toElmEncoderSource (Proxy :: Proxy NewEpisode)

    ] ++ generateElmForAPI (Proxy :: Proxy Api))

main :: IO ()
main = specsToDir [spec] "../frontend"
