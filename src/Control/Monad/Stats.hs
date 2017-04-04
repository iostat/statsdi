{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Monad.Stats
    ( module Export
    , StatsT(..)
    , runStatsT
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception              (AsyncException (..),
                                                 fromException)
import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Lift.Local
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Char8          as Char8
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import           Data.IORef
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import qualified Network.Socket                 as Socket hiding (recv,
                                                           recvFrom, send,
                                                           sendTo)
import qualified Network.Socket.ByteString      as Socket

import           Control.Monad.Stats.Ethereal   as Export
import           Control.Monad.Stats.TH         as Export
import           Control.Monad.Stats.Types      as Export


import           Data.Proxy

-- sample usage
--
-- defineCounter "vm.txs_processed" [("vm_type", "evm")]
-- defineTimer   "vm.run_loop"      [("vm_type", "evm")]
-- # quasiquotes out to
-- # vm_txs_processed = Counter { counterName = "vm.txs_processed", counterTags = [("vm_type", "evm")] }
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

mkStatsDSocket :: (MonadIO m, Monad m) => StatsTConfig -> m (TMVar Socket.Socket, Socket.SockAddr)
mkStatsDSocket cfg = do
    addrInfos <- liftIO $ Socket.getAddrInfo Nothing (Just $ host cfg) (Just . show $ port cfg)
    case addrInfos of
        []    -> error $ "Unsupported address: " ++ host cfg ++ ":" ++ show (port cfg)
        (a:_) -> liftIO $ do
         sock <- Socket.socket (Socket.addrFamily a) Socket.Datagram Socket.defaultProtocol
         tmv <- atomically (newTMVar sock)
         return (tmv, Socket.addrAddress a)


forkStatsThread :: (MonadIO m, Monad m) => StatsTEnvironment -> m ThreadId
forkStatsThread env@(StatsTEnvironment (cfg, socket, state)) = liftIO $ do
    me <- myThreadId
    forkFinally loop $ \e -> do
        borrowTMVar socket $ \s -> do
            isConnected <- Socket.isConnected s
            when isConnected $ Socket.close s
        case e of
            Left e -> case (fromException e :: Maybe AsyncException) of
                Just ThreadKilled -> return ()
                _                 -> throwTo me e
            Right () -> return ()
    where loop = forever $ do
              let interval = flushInterval cfg
              start <- getMicrotime
              reportSamples env
              end   <- getMicrotime
              threadDelay (interval * 1000 - fromIntegral (end - start))

          getMicrotime :: (MonadIO m) => m Int
          getMicrotime = (round . (* 1000000.0) . toDouble) <$> liftIO getPOSIXTime
              where toDouble = realToFrac :: Real a => a -> Double

reportSamples :: MonadIO m => StatsTEnvironment -> m ()
reportSamples (StatsTEnvironment (cfg, socket, state)) = do
    samples <- getAndWipeStates
    borrowTMVar socket $ \sock -> forM_ samples (reportSample sock)
    where reportSample :: (MonadIO m) => Socket.Socket -> (MetricStoreKey, MetricStore) -> m ()
          reportSample sock (key, value) = void . liftIO $ Socket.send sock message
              where message    = ByteString.concat [keyName key, ":", value', keyKind key, sampleRate, tagSep, allTags]
                    value'     = Char8.pack . show $ metricValue value
                    sampleRate = if isHistogram key
                                 then ByteString.concat ["|@", Char8.pack . show $ histogramSampleRate key]
                                 else ""
                    tagSep     = if ByteString.null allTags then "" else "|#"
                    allTags    = ByteString.intercalate "," $ mybRendered ++ mybOwnTags
                        where mybRendered = case defaultRenderedTags of
                                "" -> []
                                x  -> [x]
                              mybOwnTags = case renderTags (keyTags key) of
                                "" -> []
                                x  -> [x]

          getAndWipeStates :: (MonadIO m) => m [(MetricStoreKey, MetricStore)]
          getAndWipeStates = liftIO . atomicModifyIORef' state $ \(StatsTState m) ->
                (StatsTState HashMap.empty, HashMap.toList m)

          defaultRenderedTags :: ByteString
          defaultRenderedTags = renderTags (defaultTags cfg)

          renderTags :: Tags -> ByteString
          renderTags = ByteString.intercalate "," . map renderTag
              where renderTag :: Tag -> ByteString
                    renderTag (k, v) = ByteString.concat [k, ":", v]


type StatsT t m a = ReaderT t StatsTEnvironment m a

runStatsT :: (Monad m, MonadIO m) => proxy t -> StatsT t m a -> StatsTConfig -> m a
runStatsT t m c = do
    (socket, addr) <- mkStatsDSocket c
    liftIO $ borrowTMVar socket (`Socket.connect` addr)
    theEnv <- mkStatsTEnv c socket
    flip (runReaderT t) theEnv $ do
        tid <- forkStatsThread theEnv
        ret <- m
        reportSamples theEnv -- just in case our actions ran faster than 2x flush interval
        liftIO $ killThread tid
        return ret