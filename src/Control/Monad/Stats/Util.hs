module Control.Monad.Stats.Util where

import           Control.Monad.IO.Class
import           Data.Time.Clock.POSIX

getMicrotime :: (MonadIO m) => m Int
getMicrotime = (round . (* 1000000.0) . toDouble) <$> liftIO getPOSIXTime

getMillitime :: (MonadIO m) => m Int
getMillitime = (round . (* 1000.0) . toDouble) <$> liftIO getPOSIXTime

toDouble :: (Real n) => n -> Double
toDouble = realToFrac