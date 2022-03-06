{-# LANGUAGE RecordWildCards #-}

module Feedback.Common.Process where

import Data.Map as M
import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed as Typed
import UnliftIO

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !P,
    processHandleWaiter :: Async ()
  }

type P = Process () () ()

startProcessAndWait :: Map String String -> String -> IO ExitCode
startProcessAndWait extraEnv command = do
  processConfig <- makeProcessConfigFor extraEnv command
  startProcess processConfig >>= waitExitCode

startProcessHandle :: (ExitCode -> IO ()) -> Map String String -> String -> IO ProcessHandle
startProcessHandle waiterFunc extraEnv command = do
  processConfig <- makeProcessConfigFor extraEnv command
  processHandleProcess <- startProcess processConfig
  processHandleWaiter <- async $ do
    ec <- waitExitCode processHandleProcess
    waiterFunc ec
  pure ProcessHandle {..}

makeProcessConfigFor :: Map String String -> String -> IO (ProcessConfig () () ())
makeProcessConfigFor extraEnv command = do
  env <- getEnvironment
  let envForProcess = M.toList $ M.union extraEnv (M.fromList env)
  pure $
    setStdout inherit
      . setStderr inherit
      . setStdin closed -- TODO make this configurable?
      . setEnv envForProcess
      $ shell command

stopProcessHandle :: ProcessHandle -> IO ()
stopProcessHandle ProcessHandle {..} = do
  stopProcess processHandleProcess
  -- No need to cancel the waiter thread.
  pure ()
