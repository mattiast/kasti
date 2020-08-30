{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module ServantStuff
  ( app,
    server,
  )
where

import Api
import Context
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Pool
import Database.PostgreSQL.Simple
import EpisodeDb
import GetFeed (syncFeeds)
import Network.Wai
import Servant hiding (Context)
import Types

app :: Context -> Middleware
app context application =
  let x = server context
   in serve (Proxy :: Proxy (Api :<|> Raw)) (x :<|> Tagged application)

server :: Context -> (Server Api)
server context =
  let a = progressStuff context
      b = feedsStuff context
      c = feedStuff context
      d = episodeStuff context
      e = syncfeedStuff context
   in a :<|> b :<|> c :<|> d :<|> e

withConn :: (Connection -> IO a) -> (Context -> IO a)
withConn thing context = withResource (cPool context) thing

progressStuff :: Context -> (Server ProgressApi)
progressStuff context =
  (\episodeId -> fmap noCache $ (liftIO ( withConn (readPosition episodeId) context)))
    :<|> fmap noCache (liftIO ( withConn readPositions context))
    :<|> (\prog -> liftIO (( withConn) (writePosition prog) context >> return ()))

feedsStuff :: Context -> (Server FeedsApi)
feedsStuff = liftIO . withConn readFeeds

feedStuff :: Context -> (Server FeedApi)
feedStuff context fi = liftIO $ withConn (writeFeed fi) context >> return ()

episodeStuff :: Context -> (Server EpisodeApi)
episodeStuff context =
  (fmap noCache $ liftIO $ withConn (readNewEpisodes 15) context)
    :<|> (\feedId -> fmap noCache $ liftIO $ withConn (readEpisodes feedId) context)

syncfeedStuff :: Context -> (Server SyncFeedApi)
syncfeedStuff context = (liftIO $ syncAll context) :<|> (\feedId -> liftIO $ (syncOne feedId) context)

syncAll :: Context -> IO Int
syncAll conn = do
  fs <- withConn readFeeds conn
  runReaderT (syncFeeds [(fid, fi) | FStuff fid fi <- fs]) conn

syncOne :: FeedId -> Context -> IO Int
syncOne feedId conn = do
  mf <- withConn (readFeed feedId) conn
  runReaderT (syncFeeds (fmap (feedId,) mf)) conn
