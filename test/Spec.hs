{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Stats
import           Data.Proxy

import           Harness
import Debug.Trace

data MyTag = MyTag
myTag :: Proxy MyTag
myTag = Proxy

myCtr :: Counter
myCtr = Counter "hello.world" []

taggedCtr :: Counter
taggedCtr = Counter "bye.world" [("sometag","testing")]

main :: IO ()
main = print =<< runStatsTCapturingOutput myTag action defaultStatsTConfig 5000
    where action = do
              liftIO $ putStrLn ""
              liftIO $ putStrLn "action/tick"
              tick myTag myCtr
              liftIO $ putStrLn "action/delay"
              liftIO $ threadDelay 2000000
              liftIO $ putStrLn "action/tickBy"
              tickBy myTag 10 taggedCtr
              liftIO $ putStrLn "action/delayAgain"
              liftIO $ threadDelay 500000
              liftIO $ putStrLn "action/return"
