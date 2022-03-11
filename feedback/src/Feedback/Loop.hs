{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feedback.Loop where

import Control.Monad
import qualified Data.Text as T
import Data.Word
import Feedback.Common.OptParse
import Feedback.Common.Output
import Feedback.Common.Process
import Feedback.Loop.Filter
import Feedback.Loop.OptParse
import GHC.Clock (getMonotonicTimeNSec)
import Path
import Path.IO
import System.Exit
import System.FSNotify as FS
import System.IO (hGetChar)
import System.Mem (performGC)
import Text.Colour
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
import UnliftIO

runFeedbackLoop :: IO ()
runFeedbackLoop = do
  -- The outer loop happens here, before 'getLoopSettings'
  -- so that the loop can be the thing that's being worked on as well.
  here <- getCurrentDir
  mStdinFiles <- getStdinFiles here
  forever $ do
    LoopSettings {..} <- getLoopSettings
    eventChan <- newChan
    outputChan <- newChan
    -- 0.1 second debouncing, 0.001 was too little
    let conf = FS.defaultConfig {confDebounce = Debounce 0.1}
    FS.withManagerConf conf $ \watchManager -> do
      eventFilter <- mkEventFilter here mStdinFiles loopSettingFilterSettings
      stopListeningAction <-
        FS.watchTree
          watchManager
          (fromAbsDir here) -- Where to watch
          eventFilter
          $ \event -> do
            writeChan eventChan event
      race_
        (processWorker loopSettingRunSettings eventChan outputChan)
        (outputWorker loopSettingOutputSettings outputChan)
      stopListeningAction

data RestartEvent = FSEvent !FS.Event | StdinEvent !Char
  deriving (Show, Eq)

processWorker :: RunSettings -> Chan FS.Event -> Chan Output -> IO ()
processWorker runSettings eventChan outputChan = do
  let sendOutput = writeChan outputChan
  -- Record starting time
  start <- getMonotonicTimeNSec
  -- Start process
  processHandle <- startProcessHandle runSettings
  -- Output that the process has started
  sendOutput $ OutputProcessStarted runSettings

  -- So we don't need idle gc
  performGC

  -- Either wait for it to finish or wait for an event
  eventOrDone <-
    race
      (waitForEvent eventChan)
      (waitProcessHandle processHandle)
  case eventOrDone of
    -- If An event happened first, output it and kill the process.
    Left event -> do
      -- Output the event that has fired
      sendOutput $ OutputEvent event
      -- Output that killing will start
      sendOutput OutputKilling
      -- Kill the process
      stopProcessHandle processHandle
      -- Output that the process has been killed
      sendOutput OutputKilled
      -- Wait for the process to finish (should have by now)
      ec <- waitProcessHandle processHandle
      -- Record the end time
      end <- getMonotonicTimeNSec
      -- Output that the process has finished
      sendOutput $ OutputProcessExited ec (end - start)
    -- If the process finished first, show the result and wait for an event anyway
    Right ec -> do
      end <- getMonotonicTimeNSec
      sendOutput $ OutputProcessExited ec (end - start)
      -- Output the event that made the rerun happen
      event <- waitForEvent eventChan
      sendOutput $ OutputEvent event

waitForEvent :: Chan FS.Event -> IO RestartEvent
waitForEvent eventChan = do
  isTerminal <- hIsTerminalDevice stdin
  if isTerminal
    then do
      hSetBuffering stdin NoBuffering
      either id id
        <$> race
          (StdinEvent <$> hGetChar stdin)
          (FSEvent <$> readChan eventChan)
    else FSEvent <$> readChan eventChan

data Output
  = OutputEvent !RestartEvent
  | OutputKilling
  | OutputKilled
  | OutputProcessStarted !RunSettings
  | OutputProcessExited !ExitCode !Word64
  deriving (Show)

outputWorker :: OutputSettings -> Chan Output -> IO ()
outputWorker OutputSettings {..} outputChan = do
  terminalCapabilities <- getTerminalCapabilitiesFromEnv
  let put = putTimedChunks terminalCapabilities
  forever $ do
    event <- readChan outputChan
    case event of
      OutputEvent restartEvent -> do
        put $
          indicatorChunk "event:" : case restartEvent of
            FSEvent fsEvent ->
              [ case fsEvent of
                  Added {} -> fore green " added    "
                  Modified {} -> fore yellow " modified "
                  Removed {} -> fore red " removed  "
                  Unknown {} -> " unknown  ",
                chunk $ T.pack $ eventPath fsEvent
              ]
            StdinEvent c -> [chunk $ T.pack $ show c]
      OutputKilling -> put [indicatorChunk "killing"]
      OutputKilled -> put [indicatorChunk "killed"]
      OutputProcessStarted runSettings -> do
        case outputSettingClear of
          ClearScreen -> putStr "\ESCc"
          DoNotClearScreen -> pure ()
        mapM_ put $ startingLines runSettings
      OutputProcessExited ec nanosecs -> do
        put $ exitCodeChunks ec
        put $ durationChunks nanosecs
