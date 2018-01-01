{-# LANGUAGE OverloadedStrings, DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib
    ( Handler
    , start
    , stop
    ) where
import Control.Concurrent.Async(forConcurrently_)
import Control.Concurrent(ThreadId, forkIO, throwTo)
import Network.HTTP.Types(ok200)
import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
import System.Remote.Monitoring
import GetFeed(syncFeed)
import Types
import EpisodeDb
import PodEff
import Data.Pool(withResource)
import Control.Eff
import Control.Eff.Lift(Lift, runLift)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class(lift)
import Control.Exception.Base(AsyncException(..))
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)

newtype MyMonad a = MyMonad (Eff '[PodEff, Lift IO] a)
    deriving (Functor, Applicative, Monad, MonadIO)

noCache :: (Monad m) => ActionT e m ()
noCache = setHeader "Cache-Control" "no-cache, no-store, must-revalidate"

handleStuff :: KastiContext -> MyMonad a -> IO a
handleStuff context (MyMonad x) = withResource (cPool context) (\conn -> runLift $ runPod conn x)

data Handler = Handler
    { mainTid :: ThreadId
    , ekg :: Server
    , ctx :: KastiContext
    }

start :: KastiConfig -> IO Handler
start config = do
    pool <- initPool (dbString config)
    let context = KastiContext config pool
        tlsConfig = tlsSettings "/root/static/certificate.pem" "/root/static/key.pem"
        warpConfig = setPort 3000 defaultSettings
    server <- forkServer "localhost" 3001
    app <- scottyAppT (handleStuff context) $ jutska context
    mainThread <- forkIO $ runTLS tlsConfig warpConfig app
    return Handler
        { mainTid = mainThread
        , ekg = server
        , ctx = context
        }

stop :: Handler -> IO ()
stop h = do
    throwTo (mainTid h) (UserInterrupt :: AsyncException)
    closePool $ cPool $ ctx h
    throwTo (serverThreadId $ ekg h) (UserInterrupt :: AsyncException)

jutska :: KastiContext -> ScottyT L.Text MyMonad ()
jutska context = do
    let withConn = withResource (cPool context)
    get "/feeds" $ do
        noCache
        fs <- lift $ MyMonad getFeeds
        json fs
    post "/feed" $ do
        (fi :: FeedInfo) <- jsonData
        liftAndCatchIO $ withConn $ \conn -> writeFeeds conn [fi]
        json ("ok" :: String)
    get "/episodes/new" $ do
        noCache
        stuff <- liftAndCatchIO $ withConn $ readNewEpisodes 15
        json stuff
    get "/episodes/:feed_id" $ do
        noCache
        fid <- FeedId <$> param "feed_id"
        eps <- liftAndCatchIO $ withConn $ readEpisodes fid
        json eps
    get "/syncfeed/all" $ do
        fs <- lift $ MyMonad getFeeds
        let fids = map fst fs
        liftAndCatchIO $ forConcurrently_ fids (withConn . syncFeed)
        json ("ok" :: String)
    get "/syncfeed/:feed_id" $ do
        fid <- FeedId <$> param "feed_id"
        liftAndCatchIO $ withConn $ syncFeed fid
        json ("ok" :: String)
    post "/progress" $ do
        (msg :: ProgressMsg) <- jsonData
        liftAndCatchIO $ print msg
        liftAndCatchIO $ withConn $ writePosition msg
        status ok200
    get "/progress/all" $ do
        noCache
        poss <- liftAndCatchIO $ withConn readPositions
        json poss
    get "/progress/:episode_id" $ do
        noCache
        eid <- EpisodeId <$> param "episode_id"
        (pos :: ProgressMsg) <- liftAndCatchIO $ withConn $ readPosition eid
        json pos
    get "/browse" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "/root/static/browse.html"
    get "/continue" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "/root/static/browse.html"
    get "/new" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "/root/static/browse.html"
    get "/elm.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "/root/static/elm.js"
