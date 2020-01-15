{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Lib as Kasti
import Context
import qualified System.Posix.Signals as Sig
import Control.Concurrent.MVar
import Data.Aeson(decodeStrict')
import Data.Function((&))
import Options.Applicative hiding (header)
import qualified Data.ByteString as B
import System.Environment(getEnv)

main :: IO ()
main = do
    conf <- getConf
    h <- Kasti.start conf

    v <- newEmptyMVar
    let otherSigs = Sig.addSignal Sig.sigINT Sig.emptySignalSet
    _ <- Sig.installHandler Sig.sigTERM (termHandler v h) (Just otherSigs)
    _ <- readMVar v
    return ()

termHandler :: MVar () -> Kasti.Handler -> Sig.Handler
termHandler v h = Sig.CatchOnce $ do
    putStrLn "Stopping"
    Kasti.stop h
    putStrLn "Stopped"
    putMVar v ()

getConf :: IO Kasti.Config
getConf =
    Config
    <$> getEnv "DB_STRING"
    <*> getEnv "HTML_PATH"
    <*> getEnv "JS_PATH"
    <*> (read <$> getEnv "PORT")
