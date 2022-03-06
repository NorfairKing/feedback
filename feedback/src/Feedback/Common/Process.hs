{-# LANGUAGE RecordWildCards #-}

module Feedback.Common.Process where

import System.Exit
import System.Process.Typed as Typed
import UnliftIO

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !P,
    processHandleWaiter :: Async ()
  }

type P = Process () () ()

startProcessAndWait :: String -> IO ExitCode
startProcessAndWait command = do
  let processConfig = processConfigFor command
  startProcess processConfig >>= waitExitCode

startProcessHandle :: (ExitCode -> IO ()) -> String -> IO ProcessHandle
startProcessHandle waiterFunc command = do
  let processConfig = processConfigFor command
  processHandleProcess <- startProcess processConfig
  processHandleWaiter <- async $ do
    ec <- waitExitCode processHandleProcess
    waiterFunc ec
  pure ProcessHandle {..}

processConfigFor :: String -> ProcessConfig () () ()
processConfigFor =
  setStdout inherit
    . setStderr inherit
    . setStdin closed -- TODO make this configurable?
    . shell

stopProcessHandle :: ProcessHandle -> IO ()
stopProcessHandle ProcessHandle {..} = do
  stopProcess processHandleProcess
  -- No need to cancel the waiter thread.
  pure ()
