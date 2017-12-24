{-# LANGUAGE OverloadedStrings, DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib where
import Control.Monad(void)
import Control.Concurrent.Async(forConcurrently_)
import Data.Aeson(decodeStrict')
import Data.Function((&))
import Network.HTTP.Types(ok200)
import Options.Applicative hiding (header)
import qualified Data.ByteString as B
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

newtype MyMonad a = MyMonad (Eff '[PodEff, Lift IO] a)
    deriving (Functor, Applicative, Monad, MonadIO)

getConf :: IO KastiConfig
getConf = do
    let args = argument str (metavar "CONFIGFILE")
    (confPath :: FilePath) <- execParser $ info args fullDesc
    readConf confPath

readConf :: FilePath -> IO KastiConfig
readConf path = do
    bs <- B.readFile path
    decodeStrict' bs
        & maybe (fail "couldn't parse conf file") return

noCache :: (Monad m) => ActionT e m ()
noCache = setHeader "Cache-Control" "no-cache, no-store, must-revalidate"

handleStuff :: KastiContext -> MyMonad a -> IO a
handleStuff context (MyMonad x) = withResource (cPool context) (\conn -> runLift $ runPod conn x)

someFunc :: KastiContext -> IO ()
someFunc context = do
    let withConn = withResource (cPool context)
    void $ forkServer "localhost" 3001
    scottyT 3000 (handleStuff context) $ (id :: ScottyT L.Text MyMonad () -> ScottyT L.Text MyMonad ()) $ do
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
        get "/syncfeed/all" $ liftAndCatchIO $ do
            (fids :: [FeedId]) <- map fst <$> withConn readFeeds
            forConcurrently_ fids (withConn . syncFeed)
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
