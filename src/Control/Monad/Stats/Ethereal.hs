{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.HashMap.Strict       (HashMap)
import           Data.IORef
import           Data.Time                 (NominalDiffTime)
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

getSTS :: (MonadIO m, MonadStats tag m) => proxy tag -> m StatsTState
getSTS tag = asks tag envState >>= liftIO . readIORef

setSTS :: (MonadIO m, MonadStats tag m) => proxy tag -> StatsTState -> m ()
setSTS tag state = asks tag envState >>= liftIO . flip atomicWriteIORef state

updSTS :: (MonadIO m, MonadStats tag m) => proxy tag -> (StatsTState -> StatsTState) -> m ()
updSTS tag f = asks tag envState >>= liftIO . flip atomicModifyIORef' (\x -> (f x, ()))

tick :: (MonadIO m, MonadStats tag m) => proxy tag -> Counter -> m ()
tick tag = tickBy tag 1

tickBy :: (MonadIO m, MonadStats tag m) => proxy tag -> Int -> Counter -> m ()
tickBy tag n c = updSTS tag f
    where f (StatsTState m) = StatsTState $ case metricMapLookup c m of
                    Nothing -> metricMapInsert c MetricStore{metricValue = n} m
                    Just (MetricStore n')  -> metricMapInsert c (MetricStore (n' + n)) m

setRegularValue :: (MonadStats tag m, Metric m') => proxy tag -> Int -> m' -> m ()
setRegularValue tag v c = updSTS tag f
    where f (StatsTState m) = StatsTState (metricMapInsert c (fromIntegral v) m)

setCounter :: (MonadStats tag m) => proxy tag -> Int -> Counter -> m ()
setCounter = setRegularValue

setGauge :: (MonadStats tag m) => proxy tag -> Int -> Gauge -> m ()
setGauge = setRegularValue

time :: (MonadStats tag m) => proxy tag -> NominalDiffTime -> Timer -> m ()
time tag t = setRegularValue tag v
    where v = 12345
sample  :: (MonadStats tag m) => proxy tag -> Int -> Histogram -> m ()
sample = setRegularValue

reportEvent :: (MonadStats tag m) => proxy tag -> Event -> m ()
reportEvent = undefined

reportServiceCheck :: (MonadStats tag m) => proxy tag -> ServiceCheck -> m ()
reportServiceCheck = undefined
