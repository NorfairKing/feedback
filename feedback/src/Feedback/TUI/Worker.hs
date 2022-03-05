{-# LANGUAGE RecordWildCards #-}

module Feedback.TUI.Worker where

import Brick.BChan
import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Process.Typed
import Data.Word
import Feedback.TUI.Env
import Feedback.TUI.State
import GHC.Clock
import System.Process.Typed as Typed
import UnliftIO

tuiWorker :: W ()
tuiWorker = do
  doRun -- Start immediately
  requestChan <- asks envRequestChan
  forever $ do
    req <- liftIO $ readBChan requestChan
    case req of
      ReceiveEvent event -> do
        -- Immediately notify of the last-received event.
        sendResponse $ ReceivedEvent event
        doRun

doRun :: W ()
doRun = do
  currentProcessVar <- asks envCurrentProcess
  -- Stop the current process
  mCurrentProcess <- tryTakeMVar currentProcessVar
  mapM_ stopProcessHandle mCurrentProcess
  -- Start the new process
  processHandle <- startNewProcess
  putMVar currentProcessVar processHandle

stopProcessHandle :: ProcessHandle -> W ()
stopProcessHandle ProcessHandle {..} = do
  stopProcess processHandleProcess
  -- No need to cancel the waiter, it will finish automatically
  -- liftIO $ cancel processHandleWaiter
  -- TODO: figure out if that's true
  cancel processHandleStdoutReader
  cancel processHandleStderrReader
  pure ()

startNewProcess :: W ProcessHandle
startNewProcess = do
  command <- asks envCommand
  let processConfig =
        setStdout createSource
          . setStderr createSource
          . setStdin inherit
          . shell
          $ unwords command
  -- Start a new process
  processHandleProcess <- startProcess processConfig
  processHandleWaiter <- async $ waiterThread processHandleProcess
  processHandleStdoutReader <- async $ stdoutThread processHandleProcess
  processHandleStderrReader <- async $ stderrThread processHandleProcess
  sendResponse ProcessStarted
  pure ProcessHandle {..}

waiterThread :: P -> W ()
waiterThread process = do
  ec <- waitExitCode process
  sendResponse $ ProcessExited ec

stdoutThread :: P -> W ()
stdoutThread process =
  runConduit $
    getStdout process
      .| C.mapM_ (sendResponse . StdoutChunk)

stderrThread :: P -> W ()
stderrThread process =
  runConduit $
    getStdout process
      .| C.mapM_ (sendResponse . StdoutChunk)

sendResponse :: Response -> W ()
sendResponse response = do
  responseChan <- asks envResponseChan
  liftIO $ writeBChan responseChan response
