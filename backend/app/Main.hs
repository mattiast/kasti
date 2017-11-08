module Main where

import Lib
import EpisodeDb(initPool)
import Types

main :: IO ()
main = do
    conf <- getConf
    pool <- initPool (dbString conf)
    someFunc (KastiContext conf pool)
