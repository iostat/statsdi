{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
module Control.Monad.Stats.Types where

import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Char8  as Char8
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.IORef
import           Data.Time.Clock        (UTCTime)

type Tag  = (ByteString, ByteString)
type Tags = [Tag]
newtype Uid = Uid Int deriving (Eq, Ord, Read, Show)
newtype SampleRate = SampleRate Float deriving (Eq, Ord, Read, Show)

data MetricStoreKey = CounterKey   { ckMetric :: Counter   }
                    | GaugeKey     { gkMetric :: Gauge     }
                    | TimerKey     { tkMetric :: Timer     }
                    | HistogramKey { hkMetric :: Histogram }
                    | SetKey       { skMetric :: Set       }
                    deriving (Eq, Ord, Read, Show)

isHistogram :: MetricStoreKey -> Bool
isHistogram (HistogramKey _) = True
isHistogram _                = False

histogramSampleRate :: MetricStoreKey -> SampleRate
histogramSampleRate (HistogramKey h) = _histogramSampleRate h
histogramSampleRate _ = error "called histogramSampleRate on a non-HistogramKey. pls use `isHistogram` to avoid this"

newtype MetricStore = MetricStore { metricValue :: Int } deriving (Eq, Ord, Read, Show, Enum, Num, Real, Integral)

class (Eq m, Ord m, Read m, Show m) => Metric m where
    metricStoreKey :: m -> MetricStoreKey

instance Hashable MetricStoreKey where
    hashWithSalt salt m = salt `hashWithSalt` keyName m `hashWithSalt` keyTags m `hashWithSalt` keyKind m

keyName :: MetricStoreKey -> ByteString
keyName (CounterKey m)   = counterName m  -- this is literally a crime against humanity
keyName (GaugeKey m)     = gaugeName m
keyName (TimerKey m)     = timerName m
keyName (HistogramKey m) = histogramName m
keyName (SetKey m)       = setName m

keyTags :: MetricStoreKey -> Tags
keyTags (CounterKey m)   = counterTags m
keyTags (GaugeKey m)     = gaugeTags m
keyTags (TimerKey m)     = timerTags m
keyTags (HistogramKey m) = histogramTags m
keyTags (SetKey m)       = setTags m

keyKind :: MetricStoreKey -> ByteString
keyKind (CounterKey m)   = "|c"
keyKind (GaugeKey m)     = "|g"
keyKind (TimerKey m)     = "|ms"
keyKind (HistogramKey m) = "|h"
keyKind (SetKey m)       = "|s"

data Counter = Counter { counterName :: ByteString, counterTags :: Tags }
    deriving (Eq, Ord, Read, Show)

data Gauge = Gauge { gaugeName :: ByteString, gaugeTags :: Tags }
    deriving (Eq, Ord, Read, Show)

data Timer = Timer { timerName :: ByteString, timerTags :: Tags }
    deriving (Eq, Ord, Read, Show)

data Histogram = Histogram { histogramName :: ByteString , histogramTags :: Tags, _histogramSampleRate :: SampleRate }
    deriving (Eq, Ord, Read, Show)

data Set = Set { setName :: ByteString, setTags :: Tags }
    deriving (Eq, Ord, Read, Show)

data Event =
    Event { eventName      :: ByteString
          , eventTags      :: Tags
          , eventTimestamp :: Maybe UTCTime
          , eventHostname  :: Maybe ByteString
          , eventAggKey    :: Maybe ByteString
          , eventPriority  :: Maybe Priority
          , eventSource    :: Maybe ByteString
          , eventAlertType :: Maybe AlertType
          } deriving (Eq, Ord, Read, Show)

data ServiceCheck =
    ServiceCheck { svcCheckName      :: ByteString
                 , svcCheckTags      :: Tags
                 , svcCheckTimestamp :: UTCTime
                 } deriving (Eq, Ord, Read, Show)

data Priority = Normal | Low
    deriving (Eq, Ord, Read, Show)

data AlertType = Error | Warning | Info | Success
    deriving (Eq, Ord, Read, Show)

data ServiceCheckStatus = StatusOK | StatusWarning | StatusCritical | StatusUnknown
    deriving (Eq, Ord, Read, Show)

instance Metric Counter where
    metricStoreKey = CounterKey

instance Metric Gauge where
    metricStoreKey = GaugeKey

instance Metric Timer where
    metricStoreKey = TimerKey

instance Metric Histogram where
    metricStoreKey = HistogramKey

instance Metric Set where
    metricStoreKey = SetKey

data StatsTConfig =
    StatsTConfig { host          :: !String
                 , port          :: !Int
                 , flushInterval :: !Int
                 , prefix        :: !ByteString
                 , suffix        :: !ByteString
                 , defaultTags   :: !Tags
                 } deriving (Eq, Read, Show)

newtype StatsTEnvironment = StatsTEnvironment (StatsTConfig, IORef StatsTState)

envConfig :: StatsTEnvironment -> StatsTConfig
envConfig (StatsTEnvironment (a, _)) = a

envState :: StatsTEnvironment -> IORef StatsTState
envState (StatsTEnvironment (_, b)) = b

type MetricMap = HashMap MetricStoreKey MetricStore

metricMapLookup :: Metric m => m -> MetricMap -> Maybe MetricStore
metricMapLookup = HashMap.lookup . metricStoreKey

metricMapInsert :: Metric m => m -> MetricStore -> MetricMap -> MetricMap
metricMapInsert = HashMap.insert . metricStoreKey

newtype StatsTState = StatsTState { registeredMetrics :: HashMap MetricStoreKey MetricStore } deriving (Eq, Read, Show)

mkStatsTEnv :: (MonadIO m, Monad m) => StatsTConfig -> m StatsTEnvironment
mkStatsTEnv conf = liftIO $
    StatsTEnvironment . (conf,) <$> newIORef (StatsTState HashMap.empty)
