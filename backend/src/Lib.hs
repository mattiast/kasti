{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where
import Web.Scotty
import GetFeed
import Types
import EpisodeDb
import Network.HTTP.Types(status200)
import Network.Wai.Middleware.HttpAuth(basicAuth, extractBasicAuth)
import Data.Text.Lazy(pack)
import Data.Text.Lazy.Encoding(encodeUtf8)
import Data.Function((&))
import Data.ByteString.Lazy(toStrict)
import Data.Maybe(fromMaybe)

someFunc :: IO ()
someFunc = scotty 3000 $ do
    middleware $ basicAuth (\_u _p -> return True) "FooRealm"
    get "/play" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "index.html"
    get "/checkuser" $ do
        ma <- header "Authorization"
        let x = ma
                & fmap (toStrict . encodeUtf8)
                >>= extractBasicAuth
                & fromMaybe ("not", "found")
        text $ pack $ show $ x
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
        json ([] :: [String])
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
