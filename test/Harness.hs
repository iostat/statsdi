{-# LANGUAGE LambdaCase #-}
module Harness
    ( runStatsTCapturingOutput
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Stats
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString
import           Data.Time.Clock.POSIX
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv)

import Debug.Trace (traceM)

tQueueToList :: TQueue a -> IO [a]
tQueueToList = fmap reverse . loop []
    where loop read q = atomically (tryReadTQueue q) >>= \case
                Nothing -> return read
                Just x  -> loop (x:read) q

listenForUDP :: StatsTConfig -> Int -> IO [ByteString]
listenForUDP cfg lingerTime = withSocketsDo $ do
    addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) (Just (host cfg)) (Just . show $ port cfg)
    sock <- case addrInfos of
          []    -> error $ "Unsupported address: " ++ host cfg ++ ":" ++ show (port cfg)
          (a:_) -> do
              socket' <- socket (addrFamily a) Datagram defaultProtocol
              bind socket' (addrAddress a)
              return socket'
    queue <- newTQueueIO
    tid <- forkFinally (udpLoop sock queue) (const $ close sock)
    threadDelay $ lingerTime * 1000
    killThread tid

    tQueueToList queue

    where udpLoop :: Socket -> TQueue ByteString -> IO ()
          udpLoop sock queue =
              recv sock 1024 >>= atomically . writeTQueue queue >> udpLoop sock queue

forkAndListen :: StatsTConfig -> Int -> IO (TMVar [ByteString])
forkAndListen cfg lingerTime = do
    var <- newEmptyTMVarIO
    void . forkIO $ listenForUDP cfg lingerTime >>= atomically . putTMVar var
    return var

runStatsTCapturingOutput :: (MonadIO m) => proxy t -> StatsT t m a -> StatsTConfig -> Int -> m ([ByteString], a)
runStatsTCapturingOutput p m c lingerTime = do
    var <- liftIO $ forkAndListen c lingerTime
    ret <- runStatsT p m c
    val <- liftIO $ atomically (takeTMVar var)
    return (val, ret)