{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Lib as Kasti
import qualified System.Posix.Signals as Sig
import Control.Concurrent.MVar
import Data.Aeson(decodeStrict')
import Data.Function((&))
import Options.Applicative hiding (header)
import qualified Data.ByteString as B
import Types

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

getConf :: IO KastiConfig
getConf = do
    let args = argument str (metavar "CONFIGFILE")
    (confPath :: FilePath) <- execParser $ info args fullDesc
    readConf confPath

readConf :: FilePath -> IO KastiConfig
readConf path = do
    bs <- B.readFile path
    decodeStrict' bs
        & maybe (fail "couldn't parse conf file") return
