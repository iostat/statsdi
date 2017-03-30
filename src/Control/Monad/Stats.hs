{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Control.Monad.Stats
    ( MonadStats
    , Tag
    , Tags
    , Metric(..)
    , Counter(..)
    , Gauge(..)
    , Timer(..)
    , Histogram(..)
    , Set(..)
    , Event(..)
    , ServiceCheck(..)
    , Priority(..)
    , AlertType(..)
    , ServiceCheckStatus(..)
    , runStatsT
    , runStatsT'
    , DefaultStatsTTag, defaultStatsTTag
    , defineCounter
    , defineGauge
    , defineTimer
    , defineHistogram
    , defineSet
    ) where

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


import           Control.Monad.IO.Class
import           Control.Monad.Ether
import           Data.Text

import qualified Language.Haskell.TH as TH

type Tag  = (Text, Text)
type Tags = [Tag]

type DateTime = () -- todo
type NominalDiffTime = () -- todo

class Metric m where
    metricName :: m -> Text
    metricTags :: m -> Tags
    serialize  :: m -> Text -> Text

data Counter = Counter { counterName :: Text, counterTags :: Tags }
    deriving (Eq, Read, Show)

data Gauge = Gauge { gaugeName :: Text, gaugeTags :: Tags }
    deriving (Eq, Read, Show)

data Timer = Timer { timerName :: Text, timerTags :: Tags }
    deriving (Eq, Read, Show)

data Histogram = Histogram { histogramName :: Text , histogramTags :: Tags }
    deriving (Eq, Read, Show)

data Set = Set { setName :: Text, setTags :: Tags}
    deriving (Eq, Read, Show)

data Event =
    Event { eventName      :: Text
          , eventTags      :: Tags
          , eventTimestamp :: Maybe DateTime
          , eventHostname  :: Maybe Text
          , eventAggKey    :: Maybe Text
          , eventPriority  :: Maybe Priority
          , eventSource    :: Maybe Text
          , eventAlertType :: Maybe AlertType
          } deriving (Eq, Read, Show)

data ServiceCheck =
    ServiceCheck { svcCheckName      :: Text
                 , svcCheckTags      :: Tags
                 , svcCheckTimestamp :: DateTime
                 } deriving (Eq, Read, Show)

data Priority = Normal | Low
    deriving (Eq, Read, Show)

data AlertType = Error | Warning | Info | Success
    deriving (Eq, Read, Show)

data ServiceCheckStatus = StatusOK | StatusWarning | StatusCritical | StatusUnknown
    deriving (Eq, Read, Show)

instance Metric Counter where
    metricName = counterName
    metricTags = counterTags
    serialize  = undefined

instance Metric Gauge where
    metricName = gaugeName
    metricTags = gaugeTags
    serialize  = undefined

instance Metric Timer where
    metricName = timerName
    metricTags = timerTags
    serialize  = undefined

instance Metric Histogram where
    metricName = histogramName
    metricTags = histogramTags
    serialize  = undefined

instance Metric Set where
    metricName = setName
    metricTags = setTags
    serialize  = undefined

instance Metric Event where
    metricName = eventName
    metricTags = eventTags
    serialize  = undefined

instance Metric ServiceCheck where
    metricName = svcCheckName
    metricTags = svcCheckTags
    serialize  = undefined

class (MonadIO m, Monad m) => MonadStats m where
    tick        :: Counter -> m ()
    tickBy      :: Int -> Counter -> m ()
    setCounter  :: Int -> Counter -> m ()
    setGauge    :: Int -> Gauge -> m ()
    time        :: NominalDiffTime -> Timer -> m ()
    sample      :: Int -> Histogram -> m ()
    reportEvent :: Event -> m ()
    reportServiceCheck :: ServiceCheck -> m ()

data StatsTConfig = StatsTConfig
    deriving (Eq, Read, Show)

data StatsTState =
    StateTState {
                } deriving (Eq, Read, Show)

ethereal "DefaultStatsTTag" "defaultStatsTTag"

type StatsT t m a = ReaderT t StatsTConfig (StateT t StatsTState m) a
type StatsT'  m a = StatsT DefaultStatsTTag m a

defineCounter :: String -> [(String, String)] -> TH.DecsQ
defineCounter = undefined

defineGauge :: String -> [(String, String)] -> TH.DecsQ
defineGauge = undefined

defineTimer :: String -> [(String, String)] -> TH.DecsQ
defineTimer = undefined

defineHistogram :: String -> [(String, String)] -> TH.DecsQ
defineHistogram = undefined

defineSet :: String -> [(String, String)] -> TH.DecsQ
defineSet = undefined

runStatsT' :: (MonadIO m) => StatsT' m a -> m a
runStatsT' = runStatsT defaultStatsTTag

runStatsT :: (MonadIO m) => proxy t -> StatsT t m a -> m a
runStatsT = undefined