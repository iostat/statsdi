{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Stats
import qualified Control.Monad.Stats.MTL as MTLStats
import           Data.Proxy

import           Harness

data MyTag = MyTag
myTag :: Proxy MyTag
myTag = Proxy

myCtr :: Counter
myCtr = Counter "hello.world" []

ourStatsTConfig :: StatsTConfig
ourStatsTConfig = defaultStatsTConfig { flushInterval = 500 }

taggedCtr :: Counter
taggedCtr = Counter "bye.world" [("sometag","testing")]

main :: IO ()
main = runSpec >> runMTLSpec

runSpec :: IO ()
runSpec = print =<< runStatsTCapturingOutput myTag action ourStatsTConfig 5000

runMTLSpec :: IO ()
runMTLSpec = print =<< runMTLStatsTCapturingOutput mtlAction ourStatsTConfig 5000

action :: (MonadStats MyTag m) => m ()
action = do
    liftIO $ putStrLn ""
    liftIO $ putStrLn "action/tick"
    tick myTag myCtr
    liftIO $ putStrLn "action/delay"
    liftIO $ threadDelay 2000000
    liftIO $ putStrLn "action/tickBy"
    tickBy myTag 10 taggedCtr
    liftIO $ putStrLn "action/delayAgain"
    liftIO $ threadDelay 2000000
    liftIO $ putStrLn "action/return"

mtlAction :: (MTLStats.MonadStats m) => m ()
mtlAction = do
    liftIO $ putStrLn ""
    liftIO $ putStrLn "action/tick"
    MTLStats.tick myCtr
    liftIO $ putStrLn "action/delay"
    liftIO $ threadDelay 2000000
    liftIO $ putStrLn "action/tickBy"
    MTLStats.tickBy 10 taggedCtr
    liftIO $ putStrLn "action/delayAgain"
    liftIO $ threadDelay 2000000
    liftIO $ putStrLn "action/return"
