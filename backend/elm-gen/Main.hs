{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Api
import Data.Proxy
import qualified Data.Text as T
import Options.Applicative hiding (header)
import Servant.Elm
import Types

main :: IO ()
main = do
  let args = argument str (metavar "SRCDIR")
  (srcPath :: FilePath) <- execParser $ info args fullDesc
  generateElmModule
    ["Client", "Types"]
    (T.unlines
       [ ""
       , "import Time exposing (Posix)"
       , "import Iso8601"
       , defElmImports
       -- , "import Debug exposing (toString)"
       , "jsonDecPosix = Iso8601.decoder"
       , "jsonEncPosix = Iso8601.encode"
       -- , "type alias Date = Posix"
       -- , "decode = succeed"
       ])
    srcPath
    [ DefineElm (Proxy :: Proxy EStuff)
    , DefineElm (Proxy :: Proxy Episode)
    , DefineElm (Proxy :: Proxy FStuff)
    , DefineElm (Proxy :: Proxy FeedInfo)
    , DefineElm (Proxy :: Proxy NewEpisode)
    , DefineElm (Proxy :: Proxy ProgressMsg)
    , DefineElm (Proxy :: Proxy ProgressInfo)
    ]
    (Proxy :: Proxy Api)
