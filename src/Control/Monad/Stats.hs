{-# LANGUAGE DeriveFunctor         #-}
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

          getAndWipeStates :: IO ([(MetricStoreKey, MetricStore)], [NonMetricEvent])
          getAndWipeStates = atomicModifyIORef' state $ \(StatsTState m l) ->
                (StatsTState (HashMap.map (const 0) m) [], (HashMap.toList m, l))

          reportSamples :: Socket.Socket -> IO ()
          reportSamples socket = do
                (samples, nonMetrics)<- getAndWipeStates
                forM_ samples reportSample
                forM_ nonMetrics reportNonMetric
                where reportSample :: (MetricStoreKey, MetricStore) -> IO ()
                      reportSample (key, value) = void $ Socket.send socket message
                          where message    = ByteString.concat [keyName key, ":", value', keyKind key, sampleRate, tagSep, allTags]
                                value'     = Char8.pack (show value)
                                sampleRate = if isHistogram key
                                             then ByteString.concat ["|@", Char8.pack . show $ histogramSampleRate key]
                                             else ""
                                tagSep     = if ByteString.null allTags then "" else "|#"
                                allTags    = ByteString.intercalate "," [defaultRenderedTags, renderTags (keyTags key)]
                      reportNonMetric = undefined

          defaultRenderedTags :: ByteString
          defaultRenderedTags = renderTags (defaultTags cfg)

          renderTags :: Tags -> ByteString
          renderTags = ByteString.intercalate "," . map renderTag
              where renderTag :: Tag -> ByteString
                    renderTag (k, v) = ByteString.concat [k, ":", v]


data StatsT t m a = StatsT { _runStatsT :: ReaderT t StatsTEnvironment m a }
    deriving (Functor)

instance (Applicative m) => Applicative (StatsT t m) where
    pure = pure
    (<*>) = (<*>)

instance (Monad m) => Monad (StatsT t m) where
    return = return
    (>>=) = (>>=)

instance (Monad m, MonadIO m) => MonadIO (StatsT t m) where
    liftIO = liftIO

instance MonadTrans (StatsT t) where
    lift = lift

instance LiftLocal (StatsT t) where
    liftLocal = liftLocal

instance Monad m => MonadReader t StatsTEnvironment (StatsT t m) where
    ask = ask
    local = local

runStatsT :: (Monad m, MonadIO m) => proxy t -> StatsTConfig -> StatsT t m a -> m a
runStatsT t c m = do
    theEnv <- mkStatsTEnv c
    flip (runReaderT t) theEnv $ do
        tid <- forkStatsThread theEnv
        ret <- _runStatsT m
        liftIO $ killThread tid
        return ret

-- data MyTag = MyTag
-- myTag :: Proxy MyTag
-- myTag = Proxy
--
-- doATest :: IO ()
-- doATest = runStatsT myTag conf . forever $ do
--         tickBy myTag 15 ctr
--         liftIO $ threadDelay 250
--         tick myTag ctr
--         liftIO $ threadDelay 500
--     where conf = StatsTConfig { host          = "localhost"
--                               , port          = 8125
--                               , flushInterval = 1000
--                               , prefix        = ""
--                               , suffix        = ""
--                               , defaultTags   = []
--                               }
--           ctr = Counter{ counterName="derp", counterTags = []}
