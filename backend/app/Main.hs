module Main where

import Lib

main :: IO ()
main = do
    conf <- getConf
    someFunc conf
