{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib where
import Control.Lens((^?),(.~),(^.))
import Control.Monad(void)
import Control.Concurrent.Async(forConcurrently_)
import Data.Aeson((.=),decodeStrict')
import Data.Aeson.Lens
import Data.Function((&))
import Network.HTTP.Types(ok200)
import Options.Applicative hiding (header)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as S
import qualified Network.Wreq as W
import Web.Scotty
import System.Remote.Monitoring
import GetFeed
import Types
import EpisodeDb
import Data.Pool(withResource)


getToken :: KastiConfig -> S.Text -> IO (Maybe S.Text)
getToken conf session_code = do
    let (opts :: W.Options) = W.defaults & W.header "Accept" .~ ["application/json"]
    r <- W.postWith opts "https://github.com/login/oauth/access_token"
                (A.object [ "client_id" .= clientId conf
                          , "client_secret" .= clientSecret conf
                          , "code" .= session_code
                          ])
    let mAT = r ^? W.responseBody . key "access_token" . _String
    return mAT

userInfo :: S.Text -> IO (Maybe UserInfo)
userInfo token = do
    let opts = W.defaults & W.param "access_token" .~ [ token ]
    r <- W.getWith opts "https://api.github.com/user"
    let x = r ^. W.responseBody
    return $ A.decode x

getConf :: IO KastiConfig
getConf = do
    let args = argument str (metavar "CONFIGFILE")
    (confPath :: FilePath) <- execParser $ info args fullDesc
    readConf confPath

readConf :: FilePath -> IO KastiConfig
readConf path = do
    bs <- B.readFile path
    let mConf = decodeStrict' bs :: Maybe KastiConfig
    maybe (fail "couldn't parse conf file") return mConf

noCache :: ActionM ()
noCache = setHeader "Cache-Control" "no-cache, no-store, must-revalidate"

someFunc :: KastiContext -> IO ()
someFunc context = do
    let withConn = withResource (cPool context)

    void $ forkServer "localhost" 3001
    scotty 3000 $ do
        get "/feeds" $ do
            noCache
            fs <- liftAndCatchIO $ withConn readFeeds
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
