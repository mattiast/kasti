{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where
import Web.Scotty
import Data.Monoid (mconcat)
import GetFeed

someFunc :: IO ()
someFunc = scotty 3000 $ do
    get "/play" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "index.html"
    get "/feeds" $ do
        fs <- liftAndCatchIO $ withConn feedsFromDB
        json fs
    get "/episodes/:feed_id" $ do
        fid <- FeedId <$> param "feed_id"
        eps <- liftAndCatchIO $ withConn $ episodesFromDB fid
        json eps
    post "/progress" $ do
        (msg :: ProgressMsg) <- jsonData
        liftAndCatchIO $ print msg
        liftAndCatchIO $ withConn $ savePosition msg
        json ([] :: [String])
    get "/progress/:episode_id" $ do
        eid <- EpisodeId <$> param "episode_id"
        (pos :: Double) <- liftAndCatchIO $ withConn $ getPosition eid
        json pos
    get "/browse" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "browse.html"
    get "/elm.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "elm.js"
