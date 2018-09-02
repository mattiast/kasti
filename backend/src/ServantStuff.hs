{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
module ServantStuff where

import Control.Monad.IO.Class
import Network.Wai
import Servant hiding (Context)

import Api
import Types
import Context
import Data.Pool
import EpisodeDb
import Database.PostgreSQL.Simple
import Control.Monad.Reader
import GetFeed(syncFeeds)


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
    (\prog -> withConn (writePosition prog) context >> return NoContent)

feedsStuff :: Context -> (Server FeedsApi)
feedsStuff = withConn readFeeds

feedStuff :: Context -> (Server FeedApi)
feedStuff context fi = withConn (writeFeed fi) context >> return NoContent

episodeStuff :: Context -> (Server EpisodeApi)
episodeStuff context = (withConn (readNewEpisodes 15) context) :<|> (\feedId -> withConn (readEpisodes feedId) context)

syncfeedStuff :: Context -> (Server SyncFeedApi)
syncfeedStuff context = (withConn syncAll context) :<|> (\feedId -> withConn (syncOne feedId) context)

syncAll :: Connection -> IO NoContent
syncAll conn = do
    fs <- readFeeds conn
    runReaderT (syncFeeds fs) conn
    return NoContent

syncOne :: FeedId -> Connection -> IO NoContent
syncOne feedId conn = do
    mf <- readFeed feedId conn
    runReaderT (syncFeeds (fmap (feedId,) mf)) conn
    return NoContent
