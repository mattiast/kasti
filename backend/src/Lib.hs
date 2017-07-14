{-# LANGUAGE OverloadedStrings #-}
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
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
