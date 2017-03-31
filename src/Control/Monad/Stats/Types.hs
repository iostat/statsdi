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
type Uid  = ByteString

data MetricStore =
    MetricStore { metricValue :: Int
                , metricSampleRate :: Float
                } deriving (Eq, Ord, Read, Show)

data Metric = Counter   { name :: ByteString, tags :: Tags }
            | Gauge     { name :: ByteString, tags :: Tags }
            | Timer     { name :: ByteString, tags :: Tags }
            | Histogram { name :: ByteString, tags :: Tags }
            | Set       { name :: ByteString, tags :: Tags }
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

serialize :: Metric -> MetricStore -> ByteString
serialize m s =
    ByteString.snoc (serializeMetricValue m s) "|c"
serialize Gauge{..} MetricStore{..} =
    ByteString.concat [ gaugeName, ":"
                      , Char8.pack (show metricValue)
                      , "|g"
                      ]

serialize Timer{..} MetricStore{..} =
    ByteString.concat [ timerName, ":"
                      , Char8.pack (show metricValue)
                      , "|ms"
                      ]

serialize Histogram{..} MetricStore{..} =
    ByteString.concat [ histogramName, ":"
                      , Char8.pack (show metricValue)
                      , "|h"
                      , "|@", Char8.pack (show metricSampleRate)
                      ]
serialize Set{..} MetricStore{..} =
    ByteString.concat [ setName, ":"
                      , Char8.pack (show metricValue)
                      , "|s"
                      ]

metricUid :: Metric -> Uid
metricUid m = Char8.pack $ show (name m, tags m)

serializeMetricValue :: Metric -> MetricStore -> ByteString
serializeMetricValue m s = ByteString.concat [name m, ":", metricBVal]

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

metricMapLookup :: Metric -> MetricMap -> Maybe MetricStore
metricMapLookup m = Map.lookup (metricUid m)

metricMapInsert :: Metric -> MetricStore -> MetricMap -> MetricMap
metricMapInsert m = Map.insert (metricUid m)

newtype StatsTState =
    StatsTState { registeredMetrics :: MetricMap
                } deriving (Eq, Ord, Read, Show)

mkStatsTEnv :: (MonadIO m, Monad m) => StatsTConfig -> m StatsTEnvironment
mkStatsTEnv conf = liftIO $
    StatsTEnvironment . (conf,) <$> newIORef (StatsTState Map.empty)
