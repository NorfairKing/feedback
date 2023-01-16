{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module FeedbackSpec (spec) where

import Control.Concurrent
import qualified Data.ByteString as SB
import Path
import Path.IO
import System.IO
import System.Posix
import System.Process.Typed
import Test.Syd
import Test.Syd.Path

spec :: Spec
spec = sequential . tempDirSpec "feedback" . coverageSpec $ do
  let waitABit = threadDelay 250_000 -- Wait 250 ms
  it "can show help text" $ \tdir -> do
    let cp =
          setStdout nullStream
            . setWorkingDir (fromAbsDir tdir)
            $ proc "feedback" ["--help"]
    runProcess_ cp :: IO ()

  it "can start a loop and wait for input" $ \tdir -> do
    let cp =
          setStdout nullStream
            . setWorkingDir (fromAbsDir tdir)
            $ proc "feedback" ["echo", "hi"]
    withProcessTerm cp $ \ph -> do
      waitABit
      -- If the program is still running after 100ms, we assume that it is waiting.
      mExitCode <- getExitCode ph
      mExitCode `shouldBe` Nothing

  it "can run a command once, wait for input" $ \tdir ->
    withSystemTempDir "somewhere-else" $ \otherTDir -> do
      resultFile <- resolveFile otherTDir "result" -- In another dir so the loop doesn't rerun automatically
      let cp =
            setStdout nullStream
              . setWorkingDir (fromAbsDir tdir)
              $ proc "feedback" ["--", "bash", "-c", "echo hi >" <> fromAbsFile resultFile]
      withProcessTerm cp $ \ph -> do
        waitABit

        result <- SB.readFile (fromAbsFile resultFile)
        result `shouldBe` "hi\n"

        -- If the program is still running after 100ms, we assume that it is waiting.
        mExitCode <- getExitCode ph
        mExitCode `shouldBe` Nothing

  it "can run a command once, wait for manual input, and run it again upon input, and then still be running" $ \tdir ->
    withSystemTempDir "somewhere-else" $ \otherTDir -> do
      -- In another dir so the loop doesn't rerun automatically
      dateFile <- resolveFile otherTDir "datefile"

      (masterFd, slaveFd) <- openPseudoTerminal
      masterHandle <- fdToHandle masterFd
      slaveHandle <- fdToHandle slaveFd

      let cp =
            setStdout nullStream
              . setStdin (useHandleOpen slaveHandle)
              . setWorkingDir (fromAbsDir tdir)
              $ proc "feedback" ["--", "bash", "-c", "date +%N >" <> fromAbsFile dateFile]
      withProcessTerm cp $ \ph -> do
        waitABit

        -- Feedback is running.
        mExitCode <- getExitCode ph
        mExitCode `shouldBe` Nothing

        beforeContents <- SB.readFile (fromAbsFile dateFile)

        hPutChar masterHandle 'r'
        hFlush masterHandle

        waitABit

        afterContents <- SB.readFile (fromAbsFile dateFile)
        afterContents `shouldNotBe` beforeContents

  it "can run a command once, wait for a file to change, and run it again upon input, and then still be running" $ \tdir ->
    withSystemTempDir "somewhere-else" $ \otherTDir -> do
      -- In another dir so the loop doesn't rerun automatically
      dateFile <- resolveFile otherTDir "datefile"

      -- In the same dir so the loop reruns when we change it
      triggerFile <- resolveFile tdir "trigger"
      SB.writeFile (fromAbsFile triggerFile) "initial"

      (_, slaveFd) <- openPseudoTerminal
      -- masterHandle <- fdToHandle masterFd
      slaveHandle <- fdToHandle slaveFd

      let cp =
            setStdout nullStream
              . setStdin (useHandleOpen slaveHandle)
              . setWorkingDir (fromAbsDir tdir)
              $ proc "feedback" ["--no-clear", "--debug", "--", "bash", "-c", "date +%N >" <> fromAbsFile dateFile]
      withProcessTerm cp $ \ph -> do
        waitABit

        -- Feedback is running.
        mExitCode <- getExitCode ph
        mExitCode `shouldBe` Nothing

        -- Make sure the file exists now
        beforeContents <- SB.readFile (fromAbsFile dateFile)

        -- Change the trigger file, this should cause the loop to rerun.
        SB.writeFile (fromAbsFile triggerFile) "go go go"

        -- Make sure the loop is rerun
        waitABit

        -- Make sure the loop is rerun
        afterContents <- SB.readFile (fromAbsFile dateFile)
        afterContents `shouldNotBe` beforeContents

        waitABit

-- It's really annoying that this is necessary, but hear me out.
-- `feedback` is run in a tempdir.
-- When instrumented with coverage tooling, it will output 'coverage.dat' its
-- working dir.
-- We have to copy back the coverage info in order to make the coverage report.
--
-- It's slow and dirty but it works.
coverageSpec :: SpecWith (Path Abs Dir) -> SpecWith (Path Abs Dir)
coverageSpec = after $ \tdir -> do
  specificCoverageFile <- resolveFile tdir "coverage.dat"
  topLevelCoverageFile <- resolveFile' "coverage.dat"
  mSpecificCoverage <- forgivingAbsence $ SB.readFile (fromAbsFile specificCoverageFile)
  mTopLevelCoverage <- forgivingAbsence $ SB.readFile (fromAbsFile topLevelCoverageFile)
  SB.writeFile (fromAbsFile topLevelCoverageFile) $ case (mTopLevelCoverage, mSpecificCoverage) of
    (Nothing, Nothing) -> mempty
    (Just topLevelCoverage, Nothing) -> topLevelCoverage
    (Nothing, Just specificCoverage) -> specificCoverage
    (Just topLevelCoverage, Just specificCoverage) -> topLevelCoverage <> specificCoverage
