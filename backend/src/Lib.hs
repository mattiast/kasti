{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib where
import Control.Lens((^?),(.~),(^.))
import Control.Monad(join)
import Data.Aeson((.=))
import Data.Aeson.Lens
import Data.ByteString.Lazy(ByteString,toStrict)
import Data.Function((&))
import Data.Foldable(traverse_)
import Data.Maybe(fromMaybe)
import Data.Monoid((<>))
import Data.Text.Encoding(decodeUtf8)
import Network.HTTP.Types(ok200)
import Network.Wai.Middleware.HttpAuth(extractBasicAuth)
import Network.Wai.Session
import Network.Wai.Session.Map(mapStore)
import Network.Wai(vault)
import Options.Applicative hiding (header)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wreq as W
import Web.Cookie
import Web.Scotty
import GetFeed
import Types
import EpisodeDb

data KastiConfig = KastiConfig {
    clientId :: S.Text
  , clientSecret :: S.Text
} deriving Show

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

userInfo :: S.Text -> IO ByteString
userInfo token = do
    let opts = W.defaults & W.param "access_token" .~ [ token ]
    r <- W.getWith opts "https://api.github.com/user"
    let x = r ^. W.responseBody
    return x

getConf :: IO KastiConfig
getConf = do
    let args = argument str (metavar "CONFIGFILE")
    (confPath :: FilePath) <- execParser $ info args fullDesc
    bs <- B.readFile confPath
    let mcId = bs ^? key "client_id" . _String
        mcSecret = bs ^? key "client_secret" . _String
        mConf = KastiConfig <$> mcId <*> mcSecret
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
            (mUser :: Maybe ByteString) <- liftAndCatchIO $ mapM userInfo mToken
            let mName = do
                    user <- mUser
                    user ^? key "name" . _String
            mapM_ (mySessionInsert session "name") mName
            redirect "/browse"
        get "/checkuser" $ do
            mUser <- mySessionLookup session "name"
            text $ L.fromStrict $ fromMaybe "not found" $ mUser
        get "/feeds" $ do
            fs <- liftAndCatchIO $ withConn readFeeds
            json fs
        get "/episodes/:feed_id" $ do
            fid <- FeedId <$> param "feed_id"
            eps <- liftAndCatchIO $ withConn $ readEpisodes fid
            json eps
        get "/syncfeed/:feed_id" $ do
            fid <- FeedId <$> param "feed_id"
            liftAndCatchIO $ syncFeed fid
            status ok200
        post "/progress" $ do
            (msg :: ProgressMsg) <- jsonData
            liftAndCatchIO $ print msg
            liftAndCatchIO $ withConn $ writePosition msg
            status ok200
        get "/progress/:episode_id" $ do
            eid <- EpisodeId <$> param "episode_id"
            (pos :: Double) <- liftAndCatchIO $ withConn $ readPosition eid
            json pos
        get "/browse" $ do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file "browse.html"
        get "/elm.js" $ do
            setHeader "Content-Type" "application/javascript"
            file "elm.js"
