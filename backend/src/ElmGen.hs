{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Proxy
import Types
import Elm

spec :: Spec
spec =
  Spec
    ["Client", "Types"]
    [ "import Json.Decode exposing (..)"
    , "import Json.Decode.Pipeline exposing (..)"
    , "import Exts.Json.Decode exposing (..)"
    , "import Date exposing (Date)"
    , "import Json.Encode"

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
    ]

main :: IO ()
main = specsToDir [spec] "../frontend"
