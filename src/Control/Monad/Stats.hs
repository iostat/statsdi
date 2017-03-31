{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Stats
    ( module Export
    , StatsT(..)
    , runStatsT
    ) where

import           Control.Concurrent
import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.IORef
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import qualified Network.Socket               as Socket hiding (recv, recvFrom,
                                                         send, sendTo)
import qualified Network.Socket.ByteString    as Socket

import           Control.Monad.Stats.Ethereal as Export
import           Control.Monad.Stats.TH       as Export
import           Control.Monad.Stats.Types    as Export


-- sample usage
--
-- defineCounter "vm.txs_processed" [("vm_type", "evm")]
-- defineTimer   "vm.run_loop"      [("vm_type", "evm")]
-- # quasiquotes out to
-- # vm_txs_processed = Counter { counterName = "vm_type", counterTags = [("vm_type", "evm")] }
--
-- # then you just use it inside a transformed monad
-- flip runStatsT myConfig . forever $ do
--   start <- liftIO getTime
--   tickBy 20 vm_txs_processed -- whoo so fast
--   threadDelay 1000 -- crunchin real numbers here buddy!
--   tick vm_txs_processed
--   threadDelay 10000 -- oh noes a ddos
--   done <- liftIO getTime
--   time (done - start) vm_run_loop

forkStatsThread :: (MonadIO m, Monad m) => StatsTEnvironment -> m ThreadId
forkStatsThread (StatsTEnvironment (cfg, state)) = liftIO $ do
    addrInfos <- Socket.getAddrInfo Nothing (Just $ host cfg) (Just . show $ port cfg)
    socket <- case addrInfos of
        []    -> error $ "Unsupported address: " ++ host cfg
        (a:_) -> do
            sock <- Socket.socket (Socket.addrFamily a) Socket.Datagram Socket.defaultProtocol
            Socket.connect sock (Socket.addrAddress a)
            return sock
    me <- myThreadId
    forkFinally (loop socket) $ \e -> do
        Socket.close socket
        case e of
            Left x   -> throwTo me x
            Right () -> return ()
    where loop socket = do
              let interval = flushInterval cfg
              start <- getMicrotime
              reportSamples socket
              end   <- getMicrotime
              threadDelay (interval * 1000 - fromIntegral (end - start))
              loop socket

          getMicrotime :: IO Int
          getMicrotime = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
              where toDouble = realToFrac :: Real a => a -> Double

          getAndWipeStates :: IO [(Uid, Int)]
          getAndWipeStates = atomicModifyIORef' state $ \(StatsTState x) ->
                (StatsTState $ Map.map (const 0) x, Map.toList x)

          reportSamples :: Socket.Socket -> IO ()
          reportSamples socket = getAndWipeStates >>= mapM_ (reportSample socket)
              where reportSample :: Socket.Socket -> (Uid, Int) -> IO ()
                    reportSample socket ((name, tags), value) = do
                        Socket.send

          defaultRenderedTags :: ByteString
          defaultRenderedTags = renderTags (defaultTags cfg)
          renderTags :: Tags -> ByteString
          renderTags = ByteString.intercalate "," . map renderTag
              where renderTag :: Tag -> ByteString
                    renderTag (k, v) = ByteString.concat [k, ":", v]


data (Monad m, MonadIO m) => StatsT t m a = StatsT { _m :: ReaderT t StatsTEnvironment m a }

runStatsT :: (Monad m, MonadIO m) => proxy t -> StatsTConfig -> StatsT t m a -> m a
runStatsT t c StatsT{_m = m} = do
    theEnv <- mkStatsTEnv c
    flip (runReaderT t) theEnv $ do
        tid <- forkStatsThread theEnv
        ret <- m
        liftIO $ killThread tid
        return ret
