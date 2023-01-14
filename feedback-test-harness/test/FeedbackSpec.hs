{-# LANGUAGE NumericUnderscores #-}

module FeedbackSpec (spec) where

import Control.Concurrent
import Path
import System.Process.Typed
import Test.Syd
import Test.Syd.Path

spec :: Spec
spec = sequential . tempDirSpec "feedback" $ do
  it "can show help text" $ \tdir -> do
    let cp = setStdout nullStream $ setWorkingDir (fromAbsDir tdir) $ proc "feedback" ["--help"]
    runProcess_ cp :: IO ()

  it "can start a loop and wait for input" $ \tdir -> do
    let cp = setStdout nullStream $ setWorkingDir (fromAbsDir tdir) $ proc "feedback" ["echo", "hi"]
    withProcessTerm cp $ \ph -> do
      threadDelay 100_000 -- Wait 100 ms
      -- If the program is still running after 100ms, we assume that it is waiting.
      mExitCode <- getExitCode ph
      mExitCode `shouldBe` Nothing
