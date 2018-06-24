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
import Database.PostgreSQL.Simple
import Data.Aeson hiding (json)
import Control.Monad.Freer
import Control.Monad.Reader
import Control.Monad.Trans.Class(lift)
import Control.Exception.Base(AsyncException(..))
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import System.FilePath((</>))

data Config = Config {
    dbString :: String
  , staticPath :: FilePath
  , tlsCertPath :: FilePath
  , tlsKeyPath :: FilePath
} deriving Show

data Context = Context {
    cConfig :: Config
  , cPool :: Pool Connection
}

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: "postgres_string"
        <*> v .: "static_path"
        <*> v .: "tls_cert_path"
        <*> v .: "tls_key_path"
    parseJSON _ = mempty

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
        tlsConfig = tlsSettings (tlsCertPath config) (tlsKeyPath config)
        warpConfig = setPort 3000 defaultSettings
    server <- forkServer "0.0.0.0" 3001
    app <- scottyAppT (handleStuff context) $ jutska context
    mainThread <- async $ runTLS tlsConfig warpConfig app
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
    let withConn = withResource (cPool context)
        servePage = do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ (staticPath $ cConfig context) </> "browse.html"
    get "/browse" servePage
    get "/continue" servePage
    get "/new" servePage
    get "/elm.js" $ do
        setHeader "Content-Type" "application/javascript"
        file $ (staticPath $ cConfig context) </> "elm.js"
    get "/feeds" $ do
        noCache
        fs <- lift $ MyMonad getFeeds
        json fs
    post "/feed" $ do
        (fi :: FeedInfo) <- jsonData
        lift $ MyMonad (saveFeedInfo fi)
        json ("ok" :: String)
    get "/episodes/new" $ do
        noCache
        stuff <- lift $ MyMonad (getNewEpisodes 15)
        json stuff
    get "/episodes/:feed_id" $ do
        noCache
        fid <- param "feed_id"
        eps <- lift $ MyMonad $ getEpisodes fid
        json eps
    post "/syncfeed/all" $ do
        fs <- lift $ MyMonad getFeeds
        liftAndCatchIO $ withConn $ runReaderT $ syncFeeds fs
        json ("ok" :: String)
    post "/syncfeed/:feed_id" $ do
        fid <- param "feed_id"
        mfi <- lift $ MyMonad (getFeedInfo fid)
        let fs = fmap (fid,) mfi
        liftAndCatchIO $ withConn $ runReaderT $ syncFeeds fs
        json ("ok" :: String)
    post "/progress" $ do
        (prog :: ProgressMsg) <- jsonData
        liftAndCatchIO $ print prog
        lift $ MyMonad (saveProgress prog)
        status ok200
    get "/progress/all" $ do
        noCache
        poss <- lift $ MyMonad getPositions
        json poss
    get "/progress/:episode_id" $ do
        noCache
        eid <- param "episode_id"
        (pos :: ProgressMsg) <- lift $ MyMonad (getPosition eid)
        json pos
