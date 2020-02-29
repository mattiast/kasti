{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module ServantStuff
  ( app
  , server
  ) where

import Control.Monad.IO.Class
import Network.Wai
import Servant hiding (Context)

import Api
import Context
import Control.Monad.Reader
import Data.Pool
import Database.PostgreSQL.Simple
import EpisodeDb
import GetFeed (syncFeeds)
import Types

app :: Context -> Middleware
app context application = let
    x = server context
    in serve (Proxy :: Proxy (Api :<|> Raw)) (x :<|> Tagged application)

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
    (\episodeId -> fmap noCache $ withConn (readPosition episodeId) context) :<|>
    fmap noCache (withConn readPositions context) :<|>
    (\prog -> withConn (writePosition prog) context >> return ())

feedsStuff :: Context -> (Server FeedsApi)
feedsStuff = withConn readFeeds

feedStuff :: Context -> (Server FeedApi)
feedStuff context fi = withConn (writeFeed fi) context >> return ()

episodeStuff :: Context -> (Server EpisodeApi)
episodeStuff context =
    (fmap noCache $ withConn (readNewEpisodes 15) context) :<|>
    (\feedId -> fmap noCache $ withConn (readEpisodes feedId) context)

syncfeedStuff :: Context -> (Server SyncFeedApi)
syncfeedStuff context = (withConn syncAll context) :<|> (\feedId -> withConn (syncOne feedId) context)

syncAll :: Connection -> IO ()
syncAll conn = do
    fs <- readFeeds conn
    runReaderT (syncFeeds [ (fid, fi) | FStuff fid fi <- fs ]) conn
    return ()

syncOne :: FeedId -> Connection -> IO ()
syncOne feedId conn = do
    mf <- readFeed feedId conn
    runReaderT (syncFeeds (fmap (feedId,) mf)) conn
    return ()
