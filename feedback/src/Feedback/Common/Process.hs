{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Common.Process where

import Control.Monad
import Data.List
import qualified Data.Text as T
import Data.Time
import Feedback.Common.OptParse
import Feedback.Loop.OptParse
import Path
import Path.IO
import System.Exit
import System.FSNotify as FS
import System.Process.Typed as Typed
import Text.Colour
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
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
