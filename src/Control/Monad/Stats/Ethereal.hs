{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module Control.Monad.Stats.Ethereal
    ( MonadStats
    , tick
    , tickBy
    , setCounter
    , setGauge
    , time
    , sample
    , reportEvent
    , reportServiceCheck
    ) where

import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Control.Monad.Stats.Types
import           Data.IORef
import           Data.Map.Strict           (Map)
import           Data.Time                 (NominalDiffTime)

type MonadStats t m = (Monad m, MonadIO m, MonadReader t StatsTEnvironment m)

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
            Just MetricStore{..}  -> metricMapInsert c MetricStore{metricValue = n + metricValue} m

lolicantthinkofnames :: (MonadStats tag m, Metric m') => proxy tag -> Int -> m' -> m ()
lolicantthinkofnames tag v c = updSTS tag f
    where f (StatsTState m) = StatsTState $ metricMapInsert c MetricStore{metricValue = v} m

setCounter         :: (MonadStats tag m) => proxy tag -> Int -> Counter -> m ()
setCounter = lolicantthinkofnames

setGauge           :: (MonadStats tag m) => proxy tag -> Int -> Gauge -> m ()
setGauge = lolicantthinkofnames

time               :: (MonadStats tag m) => proxy tag -> NominalDiffTime -> Timer -> m ()
time tag t = lolicantthinkofnames tag v
    where v = 12345
sample             :: (MonadStats tag m) => proxy tag -> Int -> Histogram -> m ()
sample = lolicantthinkofnames

reportEvent        :: (MonadStats tag m) => proxy tag -> Event -> m ()
reportEvent        = undefined

reportServiceCheck :: (MonadStats tag m) => proxy tag -> ServiceCheck -> m ()
reportServiceCheck = undefined
