{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Feedback.Common.Process where

import Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Feedback.Common.OptParse
import Path
import Path.IO
import System.Environment as System (getEnvironment)
import System.Exit
import qualified System.Process as Process
import System.Process.Typed as Typed
import UnliftIO.IO.File

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !P
  }

type P = Process () () ()

startProcessAndWait :: RunSettings -> IO ExitCode
startProcessAndWait runSettings = do
  processConfig <- makeProcessConfigFor runSettings
  startProcess processConfig >>= waitExitCode

startProcessHandle :: RunSettings -> IO ProcessHandle
startProcessHandle runSettings = do
  processConfig <- makeProcessConfigFor runSettings
  processHandleProcess <- startProcess processConfig
  pure ProcessHandle {..}

waitProcessHandle :: ProcessHandle -> IO ExitCode
waitProcessHandle ProcessHandle {..} = waitExitCode processHandleProcess

makeProcessConfigFor :: RunSettings -> IO (ProcessConfig () () ())
makeProcessConfigFor RunSettings {..} = do
  let RunSettings _ _ _ = undefined
  -- Set up the environment
  env <- System.getEnvironment
  let envForProcess = M.toList $ M.union runSettingExtraEnv (M.fromList env)
  let CommandScript script = runSettingCommand
  -- Set up the script file
  scriptFile <- do
    -- Write the script to a file
    systemTempDir <- getTempDir
    ensureDir systemTempDir
    tempDir <- createTempDir systemTempDir "feedback"
    scriptFile <- resolveFile tempDir "feedback-script.sh"
    writeBinaryFileDurableAtomic (fromAbsFile scriptFile) (TE.encodeUtf8 (T.pack script))

    -- Make the script executable
    oldPermissions <- getPermissions scriptFile
    let newPermissions = setOwnerExecutable True oldPermissions
    setPermissions scriptFile newPermissions

    pure $ fromAbsFile scriptFile

  pure
    $ setStdout inherit
      . setStderr inherit
      . setStdin nullStream -- TODO make this configurable?
      . setEnv envForProcess
      . setCreateGroup True -- See [ref:ProcessGroup]
      . maybe id (setWorkingDir . fromAbsDir) runSettingWorkingDir
    $ shell scriptFile

stopProcessHandle :: ProcessHandle -> IO ()
stopProcessHandle ProcessHandle {..} = do
  -- [tag:ProcessGroup]
  -- We create a new process group for the script we execute.
  -- The problem this solves is the following:
  -- When we execute a bash script, and want to stop it by sending a signal to
  -- it, bash does not propagate this signal to its subprocesses.
  -- To fix this, we put the bash script in a process group.
  -- Bash then creates subprocesses in the same process group.
  -- We can send signals to the entire group at once, which we do here.
  Process.interruptProcessGroupOf $ unsafeProcessHandle processHandleProcess
  stopProcess processHandleProcess
  -- No need to cancel the waiter thread.
  pure ()
