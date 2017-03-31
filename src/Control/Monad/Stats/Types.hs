{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Control.Monad.Stats.Types where

import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Char8  as Char8
import           Data.IORef
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Time.Clock        (UTCTime)

type Tag  = (ByteString, ByteString)
type Tags = [Tag]
type Uid  = (ByteString, Tags)

data MetricStore =
    MetricStore { metricValue :: Int
                , metricSampleRate :: Float
                } deriving (Eq, Ord, Read, Show)

class (Eq m, Ord m, Read m, Show m) => Metric m where
    metricName    :: m -> ByteString
    metricTags    :: m -> Tags
    serialize     :: m -> MetricStore -> ByteString
    metricTypeTag :: m -> ByteString

metricUid :: Metric m => m -> Uid
metricUid m = (metricName m, metricTags m)

data Counter = Counter { counterName :: ByteString, counterTags :: Tags, counterUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Gauge = Gauge { gaugeName :: ByteString, gaugeTags :: Tags, gaugeUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Timer = Timer { timerName :: ByteString, timerTags :: Tags, timerUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Histogram = Histogram { histogramName :: ByteString , histogramTags :: Tags, histogramUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Set = Set { setName :: ByteString, setTags :: Tags, setUid :: Uid }
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

justMetricValue :: m -> MetricStore -> ByteString
justMetricValue _ = Char8.pack . show . metricValue

instance Metric Counter where
    metricName = counterName
    metricTags = counterTags
    serialize  = justMetricValue
    metricTypeTag _ = "|c"

instance Metric Gauge where
    metricName = gaugeName
    metricTags = gaugeTags
    serialize  = justMetricValue
    metricTypeTag _ = "|g"


instance Metric Timer where
    metricName = timerName
    metricTags = timerTags
    serialize  = justMetricValue
    metricTypeTag _ = "|ms"

instance Metric Histogram where
    metricName = histogramName
    metricTags = histogramTags
    serialize _ MetricStore{..} =
        ByteString.concat [ Char8.pack (show metricValue)
                          , "|@"
                          , Char8.pack (show metricSampleRate)
                          ]
    metricTypeTag _ = "|h"

instance Metric Set where
    metricName = setName
    metricTags = setTags
    serialize  = justMetricValue
    metricTypeTag _ = "|s"

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

type MetricMap = Map Uid MetricStore

metricMapLookup :: Metric m => m -> MetricMap -> Maybe MetricStore
metricMapLookup m = Map.lookup (metricUid m)

metricMapInsert :: Metric m => m -> MetricStore -> MetricMap -> MetricMap
metricMapInsert m = Map.insert (metricUid m)

newtype StatsTState =
    StatsTState { registeredMetrics :: MetricMap
                } deriving (Eq, Ord, Read, Show)

mkStatsTEnv :: (MonadIO m, Monad m) => StatsTConfig -> m StatsTEnvironment
mkStatsTEnv conf = liftIO $
    StatsTEnvironment . (conf,) <$> newIORef (StatsTState Map.empty)
