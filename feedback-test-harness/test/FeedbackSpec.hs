{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module FeedbackSpec (spec) where

import Autodocodec.Yaml as Yaml
import Control.Concurrent
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Feedback.Common.OptParse
import Path
import Path.IO
import System.IO (hPutChar)
import System.Posix (fdToHandle, openPseudoTerminal, sigKILL, signalProcess)
import System.Process (getPid)
import System.Process.Typed
import Test.Syd
import UnliftIO
import UnliftIO.IO.File

spec :: Spec
spec =
  -- We need sequential because commands 'feedback' is run and produces a coverage.dat in the tempdir
  sequential . doNotRandomiseExecutionOrder . coverageSpec $ do
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
      withProcessKill cp $ \ph -> do
        waitABit
        -- If the program is still running after 100ms, we assume that it is waiting.
        mExitCode <- getExitCode ph
        mExitCode `shouldBe` Nothing

    it "can run a command once, wait for input" $ \tdir ->
      withSystemTempDir "somewhere-else" $ \otherTDir -> do
        resultFile <- resolveFile otherTDir "result.txt" -- In another dir so the loop doesn't rerun automatically
        let cp =
              setStdout nullStream
                . setWorkingDir (fromAbsDir tdir)
                $ proc "feedback" ["--", "bash", "-c", "echo hi >" <> fromAbsFile resultFile]
        withProcessKill cp $ \ph -> do
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
        withProcessKill cp $ \ph -> do
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
        withProcessKill cp $ \ph -> do
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

    it "can run in a git repository and ignore .gitignored files" $ \tdir -> do
      -- Set up the repository
      let runGit args =
            runProcess_
              $ setEnv []
                . setStdout nullStream
                . setStderr nullStream
                . setWorkingDir (fromAbsDir tdir)
              $ proc "git" args

      runGit ["init"]
      runGit ["config", "user.email", "you@example.com"]
      runGit ["config", "user.name", "Your Name"]

      notIgnoredFile <- resolveFile tdir "file.not-ignored"
      ignoredFile <- resolveFile tdir "file.ignored"

      gitignoreFile <- resolveFile tdir ".gitignore"
      SB.writeFile (fromAbsFile gitignoreFile) "*.ignored"

      runGit ["add", "."]
      runGit ["commit", "-m", "Initial commit"]

      SB.writeFile (fromAbsFile notIgnoredFile) "foo"
      SB.writeFile (fromAbsFile ignoredFile) "bar"

      runGit ["add", "."]
      runGit ["commit", "-m", "commit with files."]

      (_, slaveFd) <- openPseudoTerminal
      -- masterHandle <- fdToHandle masterFd
      slaveHandle <- fdToHandle slaveFd

      dateFile <- resolveFile tdir "datefile.ignored"
      let cp =
            setStdout nullStream
              . setStdin (useHandleOpen slaveHandle)
              . setWorkingDir (fromAbsDir tdir)
              $ proc "feedback" ["--no-clear", "--debug", "--", "bash", "-c", "date +%N >" <> fromAbsFile dateFile]
      withProcessKill cp $ \ph -> do
        waitABit

        -- Feedback is running.
        mExitCode <- getExitCode ph
        mExitCode `shouldBe` Nothing

        -- Make sure the file exists now
        beforeContents <- SB.readFile (fromAbsFile dateFile)

        -- Change the ignored file file, this should _not_ cause the loop to rerun.
        SB.writeFile (fromAbsFile ignoredFile) "go go go"

        -- Make sure the loop is rerun
        waitABit

        -- Make sure the loop is rerun
        middleContents <- SB.readFile (fromAbsFile dateFile)
        middleContents `shouldBe` beforeContents

        -- Change the not-ignored file file, this should cause the loop to rerun.
        SB.writeFile (fromAbsFile notIgnoredFile) "go go go"

        -- Make sure the loop is rerun
        waitABit

        -- Make sure the loop is rerun
        afterContents <- SB.readFile (fromAbsFile dateFile)
        afterContents `shouldNotBe` beforeContents

    xdescribe "fails for unknown reason inside nix build" $
      it "can run a command, the 'before-all' hook will have run before" $ \tdir ->
        withSystemTempDir "somewhere-else" $ \otherTDir -> do
          resultFile <- resolveFile otherTDir "result.txt" -- In another dir so the loop doesn't rerun automatically
          configFile <- resolveFile tdir "config.yaml"
          writeBinaryFileDurableAtomic (fromAbsFile configFile) $
            encodeYamlViaCodec $
              emptyConfiguration
                { configLoops =
                    M.singleton "sleep" $
                      (makeLoopConfiguration (CommandScript "sleep 5"))
                        { loopConfigHooksConfiguration =
                            emptyHooksConfiguration
                              { hooksConfigurationBeforeAll =
                                  Just $
                                    makeRunConfiguration
                                      (CommandScript ("echo hi > " <> fromAbsFile resultFile))
                              }
                        }
                }
          let cp =
                setStdout nullStream
                  . setWorkingDir (fromAbsDir tdir)
                  $ proc
                    "feedback"
                    [ "--config-file",
                      fromAbsFile configFile,
                      "sleep"
                    ]
          withProcessKill cp $ \ph -> do
            waitABit

            result <- SB.readFile (fromAbsFile resultFile)
            result `shouldBe` "hi\n"

            -- If the program is still running after 100ms, we assume that it is still sleeping.
            mExitCode <- getExitCode ph
            mExitCode `shouldBe` Nothing

    xdescribe "fails for unknown reason inside nix build" $
      it "can run a command once, wait for input. The after-first hook will have run after" $ \tdir ->
        withSystemTempDir "somewhere-else" $ \otherTDir -> do
          resultFile <- resolveFile otherTDir "result.txt" -- In another dir so the loop doesn't rerun automatically
          configFile <- resolveFile tdir "config.yaml"
          writeBinaryFileDurableAtomic (fromAbsFile configFile) $
            encodeYamlViaCodec $
              emptyConfiguration
                { configLoops =
                    M.singleton "say" $
                      (makeLoopConfiguration (CommandScript "echo run"))
                        { loopConfigHooksConfiguration =
                            emptyHooksConfiguration
                              { hooksConfigurationAfterFirst =
                                  Just $
                                    makeRunConfiguration
                                      (CommandScript ("echo hi > " <> fromAbsFile resultFile))
                              }
                        }
                }
          let cp =
                setStdout nullStream
                  . setWorkingDir (fromAbsDir tdir)
                  $ proc
                    "feedback"
                    [ "--config-file",
                      fromAbsFile configFile,
                      "say"
                    ]
          withProcessKill cp $ \ph -> do
            waitABit

            result <- SB.readFile (fromAbsFile resultFile)
            result `shouldBe` "hi\n"

            -- If the program is still running after 100ms, we assume that it is waiting.
            mExitCode <- getExitCode ph
            mExitCode `shouldBe` Nothing

withProcessKill :: ProcessConfig stdin stderr stdout -> (Process stdin stderr stdout -> IO a) -> IO a
withProcessKill cp func = withProcessWait cp $ \ph ->
  func ph `finally` killProcessHandle ph

killProcessHandle :: Process stdin stdout stderr -> IO ()
killProcessHandle ph = do
  mPid <- getPid (unsafeProcessHandle ph)
  mapM_ (signalProcess sigKILL) mPid

-- It's really annoying that this is necessary, but hear me out.
-- `feedback` is run in a tempdir.
-- When instrumented with coverage tooling, it will output 'coverage.dat' its
-- working dir.
-- We have to copy back the coverage info in order to make the coverage report.
--
-- It's slow and dirty but it works.
coverageSpec :: SpecWith (Path Abs Dir) -> Spec
coverageSpec = around $ \useTmpdir -> do
  topLevelCoverageFile <- resolveFile' "coverage.dat"
  mSpecificCoverage <- withSystemTempDir "feedback-test-harness" $ \tdir -> do
    specificCoverageFile <- resolveFile tdir "coverage.dat"
    useTmpdir tdir
    forgivingAbsence $ SB.readFile (fromAbsFile specificCoverageFile)
  mTopLevelCoverage <- forgivingAbsence $ SB.readFile (fromAbsFile topLevelCoverageFile)
  writeBinaryFileDurableAtomic (fromAbsFile topLevelCoverageFile) $
    case (mTopLevelCoverage, mSpecificCoverage) of
      (Nothing, Nothing) -> mempty
      (Just topLevelCoverage, Nothing) -> topLevelCoverage
      (Nothing, Just specificCoverage) -> specificCoverage
      (Just topLevelCoverage, Just specificCoverage) -> topLevelCoverage <> specificCoverage

waitABit :: IO ()
waitABit = threadDelay 250_000 -- Wait 250 ms
