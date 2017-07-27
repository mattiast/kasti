{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib where
import Web.Scotty
import GetFeed
import Types
import EpisodeDb
import Network.HTTP.Types(status200)
import Network.Wai.Middleware.HttpAuth(basicAuth, extractBasicAuth)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Text as S
import Data.Text.Encoding(decodeUtf8)
import Data.Function((&))
import Data.ByteString.Lazy(ByteString,toStrict)
import Data.Maybe(fromMaybe)
import qualified Data.ByteString as B
import Options.Applicative hiding (header)
import Data.Aeson.Lens
import qualified Data.Aeson as A
import Data.Aeson((.=))
import Control.Lens((^?),(.~),(^.))
import Data.Monoid((<>))
import qualified Network.Wreq as W

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

userName :: ActionM (Maybe S.Text)
userName = do
    ma <- header "Authorization"
    let x = ma
            & fmap (toStrict . L.encodeUtf8)
            >>= extractBasicAuth
            & fmap (decodeUtf8 . fst)
    return x


someFunc :: KastiConfig -> IO ()
someFunc conf = scotty 3000 $ do
--    middleware $ basicAuth (\_u _p -> return True) "FooRealm"
    get "/checkuser" $ do
        u <- userName
        text $ L.fromStrict $ u
            & fromMaybe "not found"
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
        status status200
    post "/progress" $ do
        (msg :: ProgressMsg) <- jsonData
        liftAndCatchIO $ print msg
        liftAndCatchIO $ withConn $ writePosition msg
        status status200
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
    get "/callback" $ do
        (code :: S.Text) <- param "code"
        mToken <- liftAndCatchIO $ getToken conf code
        user <- liftAndCatchIO $ case mToken of
            Nothing -> return ""
            Just token -> userInfo token
        text $ "callback with code " <> L.fromStrict code <> "  " <> L.decodeUtf8 user
    get "/login" $ do
        html $ mconcat
            [ "<a href=\"https://github.com/login/oauth/authorize?scope=user:email&client_id="
            , L.fromStrict (clientId conf)
            , "\">Click here</a>"
            ]
