{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import Web.Scotty
import Data.Monoid (mconcat)

someFunc :: IO ()
someFunc = scotty 3000 $ do
    get "/play" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "index.html"
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
