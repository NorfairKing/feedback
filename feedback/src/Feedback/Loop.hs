{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Loop where

import Control.Monad
import Data.List
import qualified Data.Text as T
import Feedback.Common.OptParse
import Feedback.Common.Output
import Feedback.Common.Process
import Feedback.Loop.OptParse
import Path
import Path.IO
import System.Exit
import System.FSNotify as FS
import Text.Colour
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
import UnliftIO

runFeedbackLoop :: IO ()
runFeedbackLoop = do
  LoopSettings {..} <- getLoopSettings
  eventChan <- newChan
  outputChan <- newChan
  here <- getCurrentDir
  -- 0.1 second debouncing, 0.001 was too little
  let conf = FS.defaultConfig {confDebounce = Debounce 0.1}
  FS.withManagerConf conf $ \watchManager -> do
    stopListeningAction <-
      FS.watchTree
        watchManager
        (fromAbsDir here) -- Where to watch
        (eventFilter here)
        $ \event -> do
          writeChan eventChan event
    concurrently_
      (processWorker loopSettingCommand eventChan outputChan)
      (outputWorker loopSettingOutputSettings outputChan)
    stopListeningAction

eventFilter :: Path Abs Dir -> FS.Event -> Bool
eventFilter here fsEvent =
  and
    [ -- It's not one of those files that vim makes
      (filename <$> parseAbsFile (eventPath fsEvent)) /= Just [relfile|4913|],
      not $ "~" `isSuffixOf` eventPath fsEvent,
      -- It's not a hidden file
      not $ hiddenHere here (eventPath fsEvent)
    ]

hiddenHere :: Path Abs Dir -> FilePath -> Bool
hiddenHere here filePath =
  (hidden <$> (parseAbsFile filePath >>= stripProperPrefix here)) /= Just False

hidden :: Path Rel File -> Bool
hidden = goFile
  where
    goFile :: Path Rel File -> Bool
    goFile f = isHiddenIn (parent f) f || goDir (parent f)
    goDir :: Path Rel Dir -> Bool
    goDir f
      | parent f == f = False
      | otherwise = isHiddenIn (parent f) f || goDir (parent f)

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp

processWorker :: String -> Chan FS.Event -> Chan Output -> IO ()
processWorker command eventChan outputChan = do
  let sendOutput = writeChan outputChan
  currentProcessVar <- newEmptyMVar
  let startNewProcess = do
        processHandle <-
          startProcessHandle
            (sendOutput . OutputProcessExited)
            command
        putMVar currentProcessVar processHandle
        sendOutput $ OutputProcessStarted command
  -- Start one process ahead of time
  startNewProcess
  forever $ do
    -- Output the event that made the rerun happen
    event <- readChan eventChan
    sendOutput $ OutputEvent event
    -- Kill the current process
    mCurrentProcess <- tryTakeMVar currentProcessVar
    forM_ mCurrentProcess $ \currentProcess -> do
      sendOutput OutputKilling
      stopProcessHandle currentProcess
      sendOutput OutputKilled
    startNewProcess

data Output
  = OutputEvent !FS.Event
  | OutputKilling
  | OutputKilled
  | OutputProcessStarted !String
  | OutputProcessExited !ExitCode
  deriving (Show)

outputWorker :: OutputSettings -> Chan Output -> IO ()
outputWorker OutputSettings {..} outputChan = do
  terminalCapabilities <- getTerminalCapabilitiesFromEnv
  let put = putTimedChunks terminalCapabilities
  forever $ do
    event <- readChan outputChan
    case event of
      OutputEvent fsEvent -> do
        put
          [ indicatorChunk "event:",
            case fsEvent of
              Added {} -> fore green "added    "
              Modified {} -> fore yellow "modified "
              Removed {} -> fore red "removed  "
              Unknown {} -> "unknown  ",
            chunk $ T.pack $ eventPath fsEvent
          ]
      OutputKilling -> put [indicatorChunk "killing"]
      OutputKilled -> put [indicatorChunk "killed"]
      OutputProcessStarted command -> do
        case outputSettingClear of
          ClearScreen -> putStr "\ESCc"
          DoNotClearScreen -> pure ()
        put
          [ indicatorChunk "started:",
            " ",
            commandChunk command
          ]
      OutputProcessExited ec -> put $ exitCodeChunks ec
