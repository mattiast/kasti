{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib
    ( Handler
    , start
    , stop
    , Config
    ) where
import Control.Concurrent(throwTo)
import Control.Concurrent.Async
import Web.Scotty
import System.Remote.Monitoring
import EpisodeDb
import Control.Exception.Base(AsyncException(..))
import Network.Wai.Handler.Warp (run)
import System.FilePath((</>))
import Context

import qualified ServantStuff as SS

data Handler = Handler
    { mainTid :: Async ()
    , ekg :: Server
    , ctx :: Context
    }

start :: Config -> IO Handler
start config = do
    pool <- initPool (dbString config)
    let context = Context config pool
    server <- forkServer "0.0.0.0" 3001
    appStatic <- scottyApp $ jutska context
    let app = SS.app context appStatic
    mainThread <- async $ run 3000 app
    return Handler
        { mainTid = mainThread
        , ekg = server
        , ctx = context
        }

stop :: Handler -> IO ()
stop h = do
    cancel (mainTid h)
    closePool $ cPool $ ctx h
    throwTo (serverThreadId $ ekg h) (UserInterrupt :: AsyncException)

jutska :: Context -> ScottyM ()
jutska context = do
    let servePage = do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ (staticPath $ cConfig context) </> "browse.html"
    get "/browse" servePage
    get "/continue" servePage
    get "/new" servePage
    get "/elm.js" $ do
        setHeader "Content-Type" "application/javascript"
        file $ (staticPath $ cConfig context) </> "elm.js"
