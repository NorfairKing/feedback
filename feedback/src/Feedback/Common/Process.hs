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
import System.Process.Typed as Typed
import UnliftIO
import UnliftIO.IO.File

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
  let RunSettings _ _ _ = undefined
  -- Set up the environment
  env <- System.getEnvironment
  let envForProcess = M.toList $ M.union runSettingExtraEnv (M.fromList env)
  -- Set up the command
  commandString <- case runSettingCommand of
    CommandArgs c -> pure c
    CommandScript s -> do
      -- Write the script to a file
      systemTempDir <- getTempDir
      scriptFile <- resolveFile systemTempDir "feedback-script.sh"
      writeBinaryFileDurableAtomic (fromAbsFile scriptFile) (TE.encodeUtf8 (T.pack s))
      -- Make the script executable
      oldPermissions <- getPermissions scriptFile
      let newPermissions = setOwnerExecutable True oldPermissions
      setPermissions scriptFile newPermissions

      pure $ fromAbsFile scriptFile

  pure $
    setStdout inherit
      . setStderr inherit
      . setStdin closed -- TODO make this configurable?
      . setEnv envForProcess
      . maybe id (setWorkingDir . fromAbsDir) runSettingWorkingDir
      $ shell commandString

stopProcessHandle :: ProcessHandle -> IO ()
stopProcessHandle ProcessHandle {..} = do
  stopProcess processHandleProcess
  -- No need to cancel the waiter thread.
  pure ()
