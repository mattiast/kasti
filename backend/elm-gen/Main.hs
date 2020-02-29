{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy
import Types
import Api
import Servant.Elm
import Servant.API.ContentTypes(NoContent)
import Options.Applicative hiding (header)
import qualified Data.Text.IO as T


    -- , toElmTypeSource (Proxy :: Proxy NewEpisode)
    -- , toElmDecoderSource (Proxy :: Proxy NewEpisode)
    -- , toElmEncoderSource (Proxy :: Proxy NewEpisode)

    -- , toElmTypeSource (Proxy :: Proxy NoContent)


main :: IO ()
main = do
    let args = argument str (metavar "SRCDIR")
    (srcPath :: FilePath) <- execParser $ info args fullDesc

    generateElmModule
        ["Client", "Types"]
        ""
        srcPath
        [ DefineElm (Proxy :: Proxy EStuff)
        , DefineElm (Proxy :: Proxy Episode)
        , DefineElm (Proxy :: Proxy FStuff)
        , DefineElm (Proxy :: Proxy NewEpisode)
        ]
        (Proxy :: Proxy Api)
