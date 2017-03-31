{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Control.Monad.Stats.Ethereal
    ( MonadStats
    , StatsT(..)
    , tick
    , tickBy
    , setCounter
    , setGauge
    , time
    , sample
    , reportEvent
    , reportServiceCheck
    , runStatsT
    ) where

import           Control.Concurrent
import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Control.Monad.Stats.Types
import           Data.IORef
import           Data.Map                  (Map)
import qualified Data.Map                  as Map

type MonadStats t m = (Monad m, MonadIO m, MonadReader t StatsTEnvironment m)

getSTS :: (MonadIO m, MonadStats tag m) => proxy tag -> m StatsTState
getSTS tag = asks tag envState >>= liftIO . readIORef

setSTS :: (MonadIO m, MonadStats tag m) => proxy tag -> StatsTState -> m ()
setSTS tag state = asks tag envState >>= liftIO . flip atomicWriteIORef state

updSTS :: (MonadIO m, MonadStats tag m) => proxy tag -> (StatsTState -> StatsTState) -> m ()
updSTS tag f = asks tag envState >>= liftIO . flip atomicModifyIORef' (\x -> (f x, ()))

tick :: (MonadIO m, MonadStats tag m) => proxy tag -> Counter -> m ()
tick tag = tickBy tag 1

tickBy :: (MonadIO m, MonadStats tag m) => proxy tag -> MetricStore -> Counter -> m ()
tickBy tag n c = updSTS tag f
    where f (StatsTState m) = StatsTState $ case metricMapLookup c m of
            Nothing -> metricMapInsert c n m
            Just v  -> metricMapInsert c (n + v) m

lolicantthinkofnames :: (MonadStats tag m, Metric m') => proxy tag -> MetricStore -> m' -> m ()
lolicantthinkofnames tag v c = updSTS tag f
    where f (StatsTState m) = StatsTState $ metricMapInsert c v m

setCounter         :: (MonadStats tag m) => proxy tag -> MetricStore -> Counter -> m ()
setCounter = lolicantthinkofnames

setGauge           :: (MonadStats tag m) => proxy tag -> MetricStore-> Gauge -> m ()
setGauge = lolicantthinkofnames

time               :: (MonadStats tag m) => proxy tag -> NominalDiffTime -> Timer -> m ()
time tag t = lolicantthinkofnames tag v
    where v = 12345

sample             :: (MonadStats tag m) => proxy tag -> MetricStore -> Histogram -> m ()
sample = lolicantthinkofnames

reportEvent        :: (MonadStats tag m) => proxy tag -> Event -> m ()
reportEvent        = undefined

reportServiceCheck :: (MonadStats tag m) => proxy tag -> ServiceCheck -> m ()
reportServiceCheck = undefined

forkStatsThread :: (MonadIO m, Monad m) => StatsTEnvironment -> m ThreadId
forkStatsThread = undefined

data (Monad m, MonadIO m) => StatsT t m a = StatsT { _m :: ReaderT t StatsTEnvironment m a }

runStatsT :: (Monad m, MonadIO m) => proxy t -> StatsTConfig -> StatsT t m a -> m a
runStatsT t c StatsT{_m = m} = do
    theEnv <- mkStatsTEnv c
    flip (runReaderT t) theEnv $ do
        tid <- forkStatsThread theEnv
        ret <- m
        liftIO $ killThread tid
        return ret

