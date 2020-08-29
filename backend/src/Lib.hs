{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( Handler,
    start,
    stop,
    Config,
  )
where

import Context
import Control.Concurrent.Async
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import EpisodeDb
import Network.Wai.Handler.Warp (run)
import qualified ServantStuff as SS
import System.FilePath (takeFileName)
import Web.Scotty

data Handler = Handler
  { mainTid :: Async (),
    ctx :: Context
  }

start :: Config -> IO Handler
start config = do
  pool <- initPool (dbString config)
  let context = Context config pool
  appStatic <- scottyApp $ jutska context
  let app = SS.app context appStatic
  mainThread <- async $ run (restPort config) app
  return
    Handler
      { mainTid = mainThread,
        ctx = context
      }

stop :: Handler -> IO ()
stop h = do
  cancel (mainTid h)
  closePool $ cPool $ ctx h

jutska :: Context -> ScottyM ()
jutska context = do
  let servePage = do
        stuff <- liftIO $ T.readFile (htmlPath $ cConfig context)
        html $ stuff & T.replace "$JSFILE" (T.pack jsname)
      jsname = takeFileName $ jsPath $ cConfig context
  get "/browse" servePage
  get "/continue" servePage
  get "/new" servePage
  get ("/js/:jsname") $ do
    jsn <- param "jsname"
    unless (jsn == jsname) next
    setHeader "Content-Type" "application/javascript"
    file $ (jsPath $ cConfig context)
