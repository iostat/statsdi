{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Control.Monad.Stats.MTL
    ( module Export
    , MonadStats
    , StatsT
    , runStatsT
    , tick
    , tickBy
    , setCounter
    , setGauge
    , time
    , sample
    , reportEvent
    , reportServiceCheck
    , MTLStatsT, mtlStatsT
    ) where

import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Control.Monad.Stats.TH    as Export
import           Control.Monad.Stats.Types as Export hiding (MonadStats, StatsT)
import           Data.Time.Clock           (NominalDiffTime)

import qualified Control.Monad.Stats       as Ethereal

ethereal "MTLStatsT" "mtlStatsT"

type MonadStats m = (Monad m, MonadIO m, Ethereal.MonadStats MTLStatsT m)
type StatsT m a = Ethereal.StatsT MTLStatsT m a

runStatsT :: (MonadIO m, Monad m) => StatsT m a -> StatsTConfig -> m a
runStatsT = Ethereal.runStatsT mtlStatsT

tick :: (MonadStats m) => Counter -> m ()
tick = Ethereal.tick mtlStatsT

tickBy :: (MonadStats m) => Int -> Counter -> m ()
tickBy = Ethereal.tickBy mtlStatsT

setCounter :: (MonadStats m) => Int -> Counter -> m ()
setCounter = Ethereal.setCounter mtlStatsT

setGauge :: (MonadStats m) => Int -> Gauge -> m ()
setGauge = Ethereal.setGauge mtlStatsT

time :: (MonadStats m) => NominalDiffTime -> Timer -> m ()
time = Ethereal.time mtlStatsT

sample  :: (MonadStats m) => Int -> Histogram -> m ()
sample = Ethereal.sample mtlStatsT

reportEvent :: (MonadStats m) => Event -> m ()
reportEvent = Ethereal.reportEvent mtlStatsT

reportServiceCheck :: (MonadStats m) => ServiceCheck -> m ()
reportServiceCheck = Ethereal.reportServiceCheck mtlStatsT
