{-# LANGUAGE TypeOperators #-}
module ServantStuff where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map
import Network.Wai
import Servant hiding (Context)

import Api
import Types
import Context
import Data.Pool
import EpisodeDb
import Database.PostgreSQL.Simple


app :: Context -> Application
app context = let
    x = server context
    in serve (Proxy :: Proxy Api) x

server :: Context -> (Server Api)
server context = let
    a = progressStuff context
    b = feedsStuff context
    c = feedStuff context
    d = episodeStuff context
    e = syncfeedStuff context
    in a :<|> b :<|> c :<|> d :<|> e

withConn :: (Connection -> IO a) -> (Context -> Handler a)
withConn thing context = liftIO $ withResource (cPool context) thing

progressStuff :: Context -> (Server ProgressApi)
progressStuff context =
    (\episodeId -> withConn (readPosition episodeId) context) :<|> 
    (withConn readPositions context) :<|> 
    (\prog -> withConn (writePosition prog) context)

feedsStuff :: Context -> (Server FeedsApi)
feedsStuff = withConn readFeeds

feedStuff :: Context -> (Server FeedApi)
feedStuff context fi = withConn (writeFeed fi) context

episodeStuff :: Context -> (Server EpisodeApi)
episodeStuff context = (withConn (readNewEpisodes 15) context) :<|> (\feedId -> withConn (readEpisodes feedId) context)

syncfeedStuff :: Context -> (Server SyncFeedApi)
syncfeedStuff context = (withConn syncAll context) :<|> (\feedId -> withConn syncOne context)

syncAll :: Connection -> IO ()
syncAll conn = return ()
--        fs <- lift $ MyMonad getFeeds
--        liftAndCatchIO $ withConn $ runReaderT $ syncFeeds fs
--        status ok200
--
syncOne :: Connection -> IO ()
syncOne conn = return ()

--        fid <- param "feed_id"
--        mfi <- lift $ MyMonad (getFeedInfo fid)
--        let fs = fmap (fid,) mfi
--        liftAndCatchIO $ withConn $ runReaderT $ syncFeeds fs
--        status ok200
