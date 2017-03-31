{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TupleSections         #-}
module Control.Monad.Stats.Types where

import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text

type Tag  = (Text, Text)
type Tags = [Tag]

type DateTime = () -- todo
type NominalDiffTime = () -- todo
type MetricStore = Int
type Uid = (Text, Tags)

class (Eq m, Ord m, Read m, Show m) => Metric m where
    metricName :: m -> Text
    metricTags :: m -> Tags
    serialize  :: m -> Text -> Text

metricUid :: Metric m => m -> Uid
metricUid m = (metricName m, metricTags m)

data Counter = Counter { counterName :: Text, counterTags :: Tags, counterUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Gauge = Gauge { gaugeName :: Text, gaugeTags :: Tags, gaugeUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Timer = Timer { timerName :: Text, timerTags :: Tags, timerUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Histogram = Histogram { histogramName :: Text , histogramTags :: Tags, histogramUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Set = Set { setName :: Text, setTags :: Tags, setUid :: Uid }
    deriving (Eq, Ord, Read, Show)

data Event =
    Event { eventName      :: Text
          , eventTags      :: Tags
          , eventTimestamp :: Maybe DateTime
          , eventHostname  :: Maybe Text
          , eventAggKey    :: Maybe Text
          , eventPriority  :: Maybe Priority
          , eventSource    :: Maybe Text
          , eventAlertType :: Maybe AlertType
          } deriving (Eq, Ord, Read, Show)

data ServiceCheck =
    ServiceCheck { svcCheckName      :: Text
                 , svcCheckTags      :: Tags
                 , svcCheckTimestamp :: DateTime
                 } deriving (Eq, Ord, Read, Show)

data Priority = Normal | Low
    deriving (Eq, Ord, Read, Show)

data AlertType = Error | Warning | Info | Success
    deriving (Eq, Ord, Read, Show)

data ServiceCheckStatus = StatusOK | StatusWarning | StatusCritical | StatusUnknown
    deriving (Eq, Ord, Read, Show)

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

data StatsTConfig =
    StatsTConfig { host          :: !Text
                 , port          :: !Int
                 , flushInterval :: !Int
                 , prefix        :: !Text
                 , suffix        :: !Text
                 , defaultTags   :: !Tags
                 } deriving (Eq, Read, Show)

newtype StatsTEnvironment = StatsTEnvironment (StatsTConfig, IORef StatsTState)

envConfig :: StatsTEnvironment -> StatsTConfig
envConfig (StatsTEnvironment (a, _)) = a

envState :: StatsTEnvironment -> IORef StatsTState
envState (StatsTEnvironment (_, b)) = b

type MetricMap = Map Uid Int

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
