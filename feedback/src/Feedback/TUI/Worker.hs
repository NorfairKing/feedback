{-# LANGUAGE RecordWildCards #-}

module Feedback.TUI.Worker where

import Brick.BChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Feedback.TUI.Env
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
  pure ()

startNewProcess :: W ProcessHandle
startNewProcess = do
  command <- asks envCommand
  let processConfig =
        setStdout closed
          . setStderr closed
          . setStdin inherit
          . shell
          $ unwords command
  -- Start a new process
  processHandleProcess <- startProcess processConfig
  processHandleWaiter <- async $ waiterThread processHandleProcess
  sendResponse ProcessStarted
  pure ProcessHandle {..}

waiterThread :: Typed.Process () () () -> W ()
waiterThread process = do
  ec <- waitExitCode process
  sendResponse $ ProcessExited ec

sendResponse :: Response -> W ()
sendResponse response = do
  responseChan <- asks envResponseChan
  liftIO $ writeBChan responseChan response
