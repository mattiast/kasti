{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib where
import Control.Lens((^?),(.~),(^.))
import Control.Monad(join)
import Control.Concurrent.Async(forConcurrently_)
import Data.Aeson((.=),decodeStrict')
import Data.Aeson.Lens
import Data.Function((&))
import Data.Maybe(fromMaybe)
import Network.HTTP.Types(ok200)
import Network.Wai.Session
import Network.Wai.Session.Map(mapStore)
import Network.Wai(vault)
import Options.Applicative hiding (header)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wreq as W
import Web.Cookie
import Web.Scotty
import System.Remote.Monitoring
import GetFeed
import Types
import EpisodeDb


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
    bs <- B.readFile confPath
    let mConf = decodeStrict' bs :: Maybe KastiConfig
    maybe (fail "couldn't parse conf file") return mConf

type MySession = Vault.Key (Session ActionM String S.Text)

mySessionLookup :: MySession -> String -> ActionM (Maybe S.Text)
mySessionLookup session k = do
    v <- vault <$> request
    let mLookup = fst <$> Vault.lookup session v
    fmap join $ sequence $ mLookup <*> pure k

mySessionInsert :: MySession -> String -> S.Text -> ActionM ()
mySessionInsert session k val = do
    v <- vault <$> request
    let Just sessionInsert = snd <$> Vault.lookup session v
    sessionInsert k val

someFunc :: KastiConfig -> IO ()
someFunc conf = do
    (sessionStore :: SessionStore ActionM String S.Text) <- mapStore genSessionId
    (session :: MySession) <- Vault.newKey
    let withConn = withConnection (dbString conf)
    forkServer "localhost" 3001
    scotty 3000 $ do
        middleware $ withSession sessionStore "kasti_token" def session
        get "/login" $ do
            html $ mconcat
                [ "<a href=\"https://github.com/login/oauth/authorize?scope=user:email&client_id="
                , L.fromStrict (clientId conf)
                , "\">Click here</a>"
                ]
        get "/callback" $ do
            (code :: S.Text) <- param "code"
            mToken <- liftAndCatchIO $ getToken conf code
            mapM_ (mySessionInsert session "token") mToken
            (mUser :: Maybe UserInfo) <- liftAndCatchIO $ fmap join $ mapM userInfo mToken
            liftAndCatchIO $ print mUser
            mapM_ (mySessionInsert session "name") $ userName <$> mUser
            redirect "/browse"
        get "/checkuser" $ do
            mUser <- mySessionLookup session "name"
            text $ L.fromStrict $ fromMaybe "not found" $ mUser
        get "/feeds" $ do
            fs <- liftAndCatchIO $ withConn readFeeds
            json fs
        post "/feed" $ do
            (fi :: FeedInfo) <- jsonData
            liftAndCatchIO $ withConn $ \conn -> writeFeeds conn [fi]
            json ("ok" :: String)
        get "/episodes/:feed_id" $ do
            fid <- FeedId <$> param "feed_id"
            eps <- liftAndCatchIO $ withConn $ readEpisodes fid
            json eps
        get "/syncfeed/all" $ liftAndCatchIO $ withConn $ \conn -> do
            (fids :: [FeedId]) <- fmap (map fst) $ readFeeds conn
            forConcurrently_ fids (\fid -> syncFeed fid conn)
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
            poss <- liftAndCatchIO $ withConn $ readPositions
            json poss
        get "/progress/:episode_id" $ do
            eid <- EpisodeId <$> param "episode_id"
            (pos :: ProgressMsg) <- liftAndCatchIO $ withConn $ readPosition eid
            json pos
        get "/browse" $ do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file "browse.html"
        get "/elm.js" $ do
            setHeader "Content-Type" "application/javascript"
            file "elm.js"
