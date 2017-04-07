{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Control.Monad.Stats.Monad
    ( MonadStats
    , StatsT(..)
    , runStatsT
    , runNoStatsT
    , borrowTMVar
    , tick
    , tickBy
    , setCounter
    , setGauge
    , time
    , histoSample
    , addSetMember
    , reportEvent
    , reportServiceCheck
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception         (AsyncException (..), fromException)
import           Control.Monad.Ether
import           Control.Monad.IO.Class
import           Control.Monad.Stats.Types
import           Control.Monad.Stats.Util
import           Data.ByteString           (ByteString)
import           Data.ByteString           as ByteString
import qualified Data.ByteString.Char8     as Char8
import           Data.Dequeue
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Time                 (NominalDiffTime)
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime)
import qualified Network.Socket            as Socket hiding (recv, recvFrom,
                                                      send, sendTo)
import qualified Network.Socket.ByteString as Socket

type MonadStats t m = (Monad m, MonadIO m, MonadReader t StatsTEnvironment m)

borrowTMVar :: (Monad m, MonadIO m) => TMVar a -> (a -> m b) -> m b
borrowTMVar tmvar m = do
    var <- liftIO . atomically $ takeTMVar tmvar
    v   <- m var
    liftIO . atomically $ putTMVar tmvar var
    return v

withSocket :: (MonadStats tag m) => proxy tag -> (Socket.Socket -> m ()) -> m ()
withSocket tag m = withEnvironment tag $ \e -> borrowTMVar (envSocket e) m

withEnvironment :: (MonadStats tag m) => proxy tag -> (StatsTEnvironment -> m ()) -> m ()
withEnvironment tag f = ask tag >>= \case
    NoStatsTEnvironment -> return ()
    env -> f env

withSTS :: (MonadStats tag m) => proxy tag -> (StatsTState -> StatsTState) -> m ()
withSTS tag f = withEnvironment tag $ \(StatsTEnvironment (_, _, state)) ->
    liftIO $ atomicModifyIORef' state (\x -> (f x, ()))

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

histoSample  :: (MonadStats tag m) => proxy tag -> Int -> Histogram -> m ()
histoSample = setRegularValue

renderTimestamp :: POSIXTime -> ByteString
renderTimestamp time = ByteString.concat ["|d:", Char8.pack . show $ posixToMillis time]

renderKeyedField :: ByteString -> Maybe ByteString -> ByteString
renderKeyedField x = maybe "" (\y -> ByteString.concat [x,y])

addSetMember :: (MonadStats tag m) => proxy tag -> Int -> Set -> m ()
addSetMember tag member Set{..} = withEnvironment tag $ enqueueNonMetric tag . rendered
    where rendered env = ByteString.concat [ setName
                                           , ":"
                                           , Char8.pack (show member)
                                           , "|s"
                                           , renderAllTags [defaultTags (envConfig env), setTags]
                                           ]

reportEvent :: (MonadStats tag m) => proxy tag -> Event -> m ()
reportEvent tag Event{..} = withEnvironment tag $ \env -> do
    timestamp <- case eventTimestamp of
        Just x  -> return $ renderTimestamp x
        Nothing -> renderTimestamp <$> liftIO getPOSIXTime
    enqueueNonMetric tag $ renderEvent timestamp (defaultTags $ envConfig env)
    where renderEvent ts dt = ByteString.concat [ "_e{"
                                                , Char8.pack . show $ ByteString.length eventName
                                                , ","
                                                , Char8.pack . show $ ByteString.length eventText
                                                , "}:"
                                                , eventName
                                                , "|"
                                                , eventText
                                                , ts
                                                , renderKeyedField "|h:" eventHostname
                                                , renderKeyedField "|k:" eventAggKey
                                                , renderKeyedField "|p:" (renderPriority <$> eventPriority)
                                                , renderKeyedField "|s" eventSource
                                                , renderKeyedField "|t:" (renderAlertType <$> eventAlertType)
                                                , renderAllTags [dt, eventTags]
                                                ]

reportServiceCheck :: (MonadStats tag m) => proxy tag -> ServiceCheck -> ServiceCheckValue -> m ()
reportServiceCheck t ServiceCheck{..} ServiceCheckValue{..} = ask t >>= \case
    NoStatsTEnvironment -> return ()
    env -> do
        timestamp <- case scvTimestamp of
            Just x  -> return $ renderTimestamp x
            Nothing -> renderTimestamp <$> liftIO getPOSIXTime
        enqueueNonMetric t $ renderSC timestamp
        where renderSC ts = ByteString.concat [ "_sc|"
                                              , serviceCheckName
                                              , "|"
                                              , renderServiceCheckStatus scvStatus
                                              , ts
                                              , renderKeyedField "|h:" scvHostname
                                              , renderAllTags [defaultTags (envConfig env), serviceCheckTags]
                                              , renderKeyedField "|m:" scvMessage
                                              ]

mkStatsDSocket :: (MonadIO m) => StatsTConfig -> m (TMVar Socket.Socket, Socket.SockAddr)
mkStatsDSocket cfg = do
    addrInfos <- liftIO $ Socket.getAddrInfo opts' host' port'
    case addrInfos of
        []    -> error $ "Unsupported address: " ++ host cfg ++ ":" ++ show (port cfg)
        (a:_) -> liftIO $ do
         sock <- Socket.socket (Socket.addrFamily a) Socket.Datagram Socket.defaultProtocol
         tmv <- atomically (newTMVar sock)
         return (tmv, Socket.addrAddress a)

    where host' = Just $ host cfg
          port' = Just . show $ port cfg
          opts' = Nothing

forkStatsThread :: (MonadIO m) => StatsTEnvironment -> m ThreadId
forkStatsThread env@(StatsTEnvironment (cfg, socket, state)) = do
    me <- liftIO myThreadId
    liftIO . forkFinally loop $ \e -> do
        borrowTMVar socket $ \s -> do
            isConnected <- Socket.isConnected s
            when isConnected $ Socket.close s
        case e of
            Left e -> case (fromException e :: Maybe AsyncException) of
                Just ThreadKilled -> return ()
                _                 -> throwTo me e
            Right () -> return ()
    where loop = forever $ do
              let interval = flushInterval cfg
              start <- getMicrotime
              reportSamples env
              end   <- getMicrotime
              threadDelay (interval * 1000 - fromIntegral (end - start))

reportSamples :: MonadIO m => StatsTEnvironment -> m ()
reportSamples (StatsTEnvironment (cfg, socket, state)) = do
    (samples, queuedEvents) <- getAndWipeStates
    borrowTMVar socket $ \sock -> do
        forM_ samples (reportSample sock)
        liftIO $ forM_ queuedEvents (Socket.send sock)

    where reportSample :: (MonadIO m) => Socket.Socket -> (MetricStoreKey, MetricStore) -> m ()
          reportSample sock (key, MetricStore value) = void . liftIO $ Socket.send sock message
              where message  = if value < 0
                               then ByteString.concat [messageWithValue 0, "\n", messageWithValue value]
                               else messageWithValue value
                    messageWithValue v = ByteString.concat [ keyName key, ":"
                                                           , Char8.pack (show v)
                                                           , keyKind key
                                                           , sampleRate
                                                           , renderAllTags [defaultTags cfg, keyTags key]
                                                           ]
                    value'     = Char8.pack (show value)
                    sampleRate = if isHistogram key
                                 then ByteString.concat ["|@", Char8.pack . show $ histogramSampleRate key]
                                 else ""

          getAndWipeStates :: (MonadIO m) => m ([(MetricStoreKey, MetricStore)], BankersDequeue ByteString)
          getAndWipeStates = liftIO . atomicModifyIORef' state $ \(StatsTState m q) ->
                (StatsTState HashMap.empty Data.Dequeue.empty, (HashMap.toList m, q))


type StatsT t m a = ReaderT t StatsTEnvironment m a

runStatsT :: (MonadIO m) => proxy t -> StatsT t m a -> StatsTConfig -> m a
runStatsT t m c = do
    (socket, addr) <- mkStatsDSocket c
    liftIO $ borrowTMVar socket (`Socket.connect` addr)
    theEnv <- mkStatsTEnv c socket
    flip (runReaderT t) theEnv $ do
        tid <- forkStatsThread theEnv
        ret <- m
        reportSamples theEnv -- just in case our actions ran faster than 2x flush interval
        liftIO $ killThread tid
        return ret

runNoStatsT :: (MonadIO m) => proxy t -> StatsT t m a -> m a
runNoStatsT t = flip (runReaderT t) NoStatsTEnvironment
