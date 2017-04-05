{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Stats.MTL
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as ByteString
import           Data.Proxy

import           Harness

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Options      (OptionDescription (..))
import           Test.Tasty.Runners      (NumThreads (..))

-- these tests effectively test the TH for sanity
-- otherwise the test suite wouldnt even compile
defineCounter "ctr.hello.world" []
defineCounter "ctr.tagged" [("env","test")]
defineGauge "gau.testing.things" []
defineTimer "time.to.end.of.earth" []
defineHistogram "hist.stuff.things" [] 1.0
defineSet "set.of.people" []

ourStatsTConfig :: StatsTConfig
ourStatsTConfig = defaultStatsTConfig { flushInterval = 250 }

st :: (MonadIO m) => Int -> StatsT m a -> m ([ByteString], a)
st = st' ourStatsTConfig

st' :: (MonadIO m) => StatsTConfig -> Int -> StatsT m a -> m ([ByteString], a)
st' = runStatsTCapturingOutput

sleepMs :: MonadIO m => Int -> m ()
sleepMs = liftIO . threadDelay . (1000 *)

main :: IO ()
main = do
    putStrLn ""
    let toRun = [sillyTests, counterTests, gaugeTests]
    forM toRun testSpecs >>= defaultMain . withTests

withTests :: [[TestTree]] -> TestTree
withTests = testGroup "Statsdi" . concat

sillyTests :: Spec
sillyTests = describe "The test harness" $ do
    it "should run and capture something (1s linger)" $ do
        (capture, _) <- st 1000 $ tick ctr_hello_world
        capture `shouldSatisfy` (not . null)

    it "should run and capture with a delay before the tick (250ms flushInterval / 1s linger / 500ms delay)" $ do
        (capture, _) <- st 1000 $ do
            sleepMs 500
            tick ctr_hello_world
        capture `shouldSatisfy` (not . null)

    it "should linger after the test runs and get everything (100ms linger / 5000ms+ test)" $ do
        (capture, _) <- st 100 $ do
            sleepMs 5000
            tick ctr_hello_world
        capture `shouldSatisfy` (not . null)

counterTests :: Spec
counterTests = describe "A Counter" $ do
    it "should have a suffix type of |c" $ do
        (capture, _) <- st 1000 $ setCounter 0 ctr_hello_world
        capture `shouldSatisfy` (not . null)
        capture `shouldSatisfy` (ByteString.isPrefixOf "ctr.hello.world:0|c" . head)

    it "should increment by one when calling `tick`" $ do
        (capture, _) <- st 1000 $ tick ctr_hello_world
        capture `shouldSatisfy` (not . null)
        capture `shouldSatisfy` (ByteString.isPrefixOf "ctr.hello.world:1|c" . head)

    it "should send a multi-event with a zeroing-out before being set to a negative number" $ do
        (capture, _) <- st 1000 $ setCounter (-20) ctr_hello_world
        capture `shouldSatisfy` (not . null)
        capture `shouldSatisfy` (ByteString.isPrefixOf "ctr.hello.world:0|c\nctr.hello.world:-20|c" . head)

gaugeTests :: Spec
gaugeTests = describe "A Gauge" $ do
    it "should have a suffix type of |g" $ do
        (capture, _) <- st 1000 $ setGauge 0 gau_testing_things
        capture `shouldSatisfy` (not . null)
        capture `shouldSatisfy` (ByteString.isPrefixOf "gau.testing.things:0|g" . head)

    it "should send a multi-event with a zeroing-out before being set to a negative number" $ do
        (capture, _) <- st 1000 $ setGauge (-20) gau_testing_things
        capture `shouldSatisfy` (not . null)
        capture `shouldSatisfy` (ByteString.isPrefixOf "gau.testing.things:0|g\ngau.testing.things:-20|g" . head)
