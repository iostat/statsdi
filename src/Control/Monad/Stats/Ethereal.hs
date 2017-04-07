{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Control.Monad.Stats.Ethereal
    ( MonadStats
    , borrowTMVar
    , tick
    , tickBy
    , setCounter
    , setGauge
    , time
    , sample
    , reportEvent
    , reportServiceCheck
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Control.Monad.Stats.Types
import           Control.Monad.Stats.Util
import           Data.ByteString           (ByteString)
import           Data.ByteString           as ByteString
import qualified Data.ByteString.Char8     as Char8
import           Data.Dequeue
import           Data.HashMap.Strict       (HashMap)
import           Data.IORef
import           Data.Time                 (NominalDiffTime)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Network.Socket

type MonadStats t m = (Monad m, MonadIO m, MonadReader t StatsTEnvironment m)

borrowTMVar :: (Monad m, MonadIO m) => TMVar a -> (a -> m b) -> m b
borrowTMVar tmvar m = do
    var <- liftIO . atomically $ takeTMVar tmvar
    v   <- m var
    liftIO . atomically $ putTMVar tmvar var
    return v

borrowSocket :: (MonadIO m, MonadStats tag m) => proxy tag -> (Socket -> m b) -> m b
borrowSocket tag m = asks tag envSocket >>= flip borrowTMVar m

withSTS :: (MonadStats tag m) => proxy tag -> (StatsTState -> StatsTState) -> m ()
withSTS tag f = ask tag >>= \case
    NoStatsTEnvironment -> return ()
    StatsTEnvironment (_, _, state) -> liftIO $ atomicModifyIORef' state (\x -> (f x, ()))

tick :: (MonadStats tag m) => proxy tag -> Counter -> m ()
tick tag = tickBy tag 1

tickBy :: (MonadStats tag m) => proxy tag -> Int -> Counter -> m ()
tickBy tag n c = withSTS tag f
    where f (StatsTState m q) = flip StatsTState q $ case metricMapLookup c m of
                    Nothing -> metricMapInsert c MetricStore{metricValue = n} m
                    Just (MetricStore n')  -> metricMapInsert c (MetricStore (n' + n)) m

setRegularValue :: (MonadStats tag m, Metric m') => proxy tag -> Int -> m' -> m ()
setRegularValue tag v c = withSTS tag f
    where f (StatsTState m q) = StatsTState (metricMapInsert c (fromIntegral v) m) q

enqueueNonMetric :: (MonadStats tag m) => proxy tag -> ByteString -> m ()
enqueueNonMetric tag e = withSTS tag f
    where f (StatsTState m q) = StatsTState m (pushBack q e)

setCounter :: (MonadStats tag m) => proxy tag -> Int -> Counter -> m ()
setCounter = setRegularValue

setGauge :: (MonadStats tag m) => proxy tag -> Int -> Gauge -> m ()
setGauge = setRegularValue

time :: (Real n, Fractional n, MonadStats tag m) => proxy tag -> n -> Timer -> m ()
time tag = setRegularValue tag . v
    where v = round . (* 1000.0) . toDouble

sample  :: (MonadStats tag m) => proxy tag -> Int -> Histogram -> m ()
sample = setRegularValue

reportEvent :: (MonadStats tag m) => proxy tag -> Event -> m ()
reportEvent tag = enqueueNonMetric tag . renderEvent
    where renderEvent = undefined

reportServiceCheck :: (MonadStats tag m) => proxy tag -> ServiceCheck -> ServiceCheckValue -> m ()
reportServiceCheck t ServiceCheck{..} ServiceCheckValue{..} = do
    defT <- asks t (defaultTags . envConfig)
    timestamp <- case scvTimestamp of
        Just x  -> return $ ts x
        Nothing -> ts <$> liftIO getPOSIXTime
    enqueueNonMetric t $ renderSC timestamp defT
    where renderSC ts defT = ByteString.concat [ "_sc|"
                                               , serviceCheckName
                                               , "|"
                                               , renderServiceCheckStatus scvStatus
                                               , ts
                                               , hostname
                                               , tagBit defT
                                               , message
                                               ]
          tagBit d = renderAllTags [d, serviceCheckTags]
          hostname = tagged "|h:" scvHostname
          message  = tagged "|m:" scvMessage
          ts time  = ByteString.concat ["|d:", Char8.pack . show $ posixToMillis time]
          tagged x = maybe "" (\y -> ByteString.concat [x,y])

