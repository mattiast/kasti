{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Proxy
import Types
import Api
import Elm
import Servant.Elm
import Servant(NoContent)

spec :: Spec
spec =
  Spec
    ["Client", "Types"]
    ([ defElmImports
    , "import Time exposing (Posix)"
     , "import Iso8601"
     , "import Debug exposing (toString)"
     , "decodeDate = Iso8601.decoder"
     , "type alias Date = Posix"
     , "decode = succeed"

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

    , toElmTypeSource (Proxy :: Proxy NoContent)

    ] ++ generateElmForAPI (Proxy :: Proxy Api))

main :: IO ()
main = specsToDir [spec] "../frontend/src"
