{-# LANGUAGE RecordWildCards #-}

module Feedback.Common.Process where

import Data.Map as M
import Feedback.Common.OptParse
import System.Environment as System (getEnvironment)
import System.Exit
import System.Process.Typed as Typed
import UnliftIO

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !P,
    processHandleWaiter :: Async ()
  }

type P = Process () () ()

startProcessAndWait :: RunSettings -> IO ExitCode
startProcessAndWait runSettings = do
  processConfig <- makeProcessConfigFor runSettings
  startProcess processConfig >>= waitExitCode

startProcessHandle :: (ExitCode -> IO ()) -> RunSettings -> IO ProcessHandle
startProcessHandle waiterFunc runSettings = do
  processConfig <- makeProcessConfigFor runSettings
  processHandleProcess <- startProcess processConfig
  processHandleWaiter <- async $ do
    ec <- waitExitCode processHandleProcess
    waiterFunc ec
  pure ProcessHandle {..}

makeProcessConfigFor :: RunSettings -> IO (ProcessConfig () () ())
makeProcessConfigFor RunSettings {..} = do
  env <- System.getEnvironment
  let envForProcess = M.toList $ M.union runSettingExtraEnv (M.fromList env)
  pure $
    setStdout inherit
      . setStderr inherit
      . setStdin closed -- TODO make this configurable?
      . setEnv envForProcess
      $ shell runSettingCommand

stopProcessHandle :: ProcessHandle -> IO ()
stopProcessHandle ProcessHandle {..} = do
  stopProcess processHandleProcess
  -- No need to cancel the waiter thread.
  pure ()
