{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib
    ( Handler
    , start
    , stop
    , Config
    ) where
import Control.Concurrent(throwTo)
import Control.Concurrent.Async
import Network.HTTP.Types(ok200)
import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
import System.Remote.Monitoring
import GetFeed(syncFeeds)
import Types
import EpisodeDb
import PodEff
import Data.Pool
import Control.Monad.Freer hiding (run)
import Control.Monad.Reader
import Control.Monad.Trans.Class(lift)
import Control.Exception.Base(AsyncException(..))
import Network.Wai.Handler.Warp (run)
import System.FilePath((</>))
import Context

import qualified ServantStuff as SS

newtype MyMonad a = MyMonad (Eff '[PodEff, IO] a)
    deriving (Functor, Applicative, Monad)

instance MonadIO MyMonad where
    liftIO = MyMonad . sendM

noCache :: (Monad m) => ActionT e m ()
noCache = setHeader "Cache-Control" "no-cache, no-store, must-revalidate"

handleStuff :: Context -> MyMonad a -> IO a
handleStuff context (MyMonad x) = withResource (cPool context) (\conn -> runM $ runPod conn x)

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
    appStatic <- scottyAppT (handleStuff context) $ jutska context
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

jutska :: Context -> ScottyT L.Text MyMonad ()
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
