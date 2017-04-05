{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Stats
import qualified Control.Monad.Stats.MTL as MTLStats
import           Data.Proxy

import           Harness

data MyTag = MyTag
myTag :: Proxy MyTag
myTag = Proxy

defineCounter "ctr.hello.world" []
defineCounter "ctr.bye.world" [("env","test")]
defineGauge "gau.testing.things" []
defineTimer "time.to.end.of.earth" []
defineHistogram "hist.stuff.things" [] 1.0
defineSet "set.of.people" []

ourStatsTConfig :: StatsTConfig
ourStatsTConfig = defaultStatsTConfig { flushInterval = 500 }

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
    tick myTag ctr_hello_world
    liftIO $ putStrLn "action/delay"
    -- liftIO $ threadDelay 200000
    liftIO $ putStrLn "action/tickBy"
    tickBy myTag 10 ctr_bye_world
    liftIO $ putStrLn "action/delayAgain"
    -- liftIO $ threadDelay 200000
    liftIO $ putStrLn "action/return"

mtlAction :: (MTLStats.MonadStats m) => m ()
mtlAction = do
    liftIO $ putStrLn ""
    liftIO $ putStrLn "action/tick"
    MTLStats.tick ctr_hello_world
    liftIO $ putStrLn "action/delay"
    -- liftIO $ threadDelay 200000
    liftIO $ putStrLn "action/tickBy"
    MTLStats.tickBy 10 ctr_bye_world
    liftIO $ putStrLn "action/delayAgain"
    -- liftIO $ threadDelay 2000000
    liftIO $ putStrLn "action/return"
