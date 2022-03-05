{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback where

import Control.Monad
import Data.List
import qualified Data.Text as T
import Data.Time
import Feedback.OptParse
import Path
import Path.IO
import System.Exit
import System.FSNotify as FS
import System.Process (showCommandForUser)
import System.Process.Typed as Typed
import Text.Colour
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
import UnliftIO

runFeedbackMain :: IO ()
runFeedbackMain = do
  Settings {..} <- getSettings
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
      (processWorker settingCommand eventChan outputChan)
      (outputWorker settingOutputSettings outputChan)
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

processWorker :: [String] -> Chan FS.Event -> Chan Output -> IO ()
processWorker command eventChan outputChan = do
  let sendOutput = writeChan outputChan
  currentProcessVar <- newEmptyMVar
  let startNewProcess = do
        -- Start a new process
        let processConfig =
              setStdout inherit
                . setStderr inherit
                . setStdin closed -- TODO make this configurable?
                . shell
                $ unwords command
        processHandleProcess <- startProcess processConfig
        processHandleWaiter <- async $ do
          ec <- waitExitCode processHandleProcess
          sendOutput $ OutputProcessExited ec
        putMVar currentProcessVar ProcessHandle {..}
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
      stopProcess $ processHandleProcess currentProcess
      -- No need to cancel the waiter thread.
      sendOutput OutputKilled
    startNewProcess

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !P,
    processHandleWaiter :: Async ()
  }

type P = Process () () ()

data Output
  = OutputEvent !FS.Event
  | OutputKilling
  | OutputKilled
  | OutputProcessStarted ![String]
  | OutputProcessExited !ExitCode
  deriving (Show)

outputWorker :: OutputSettings -> Chan Output -> IO ()
outputWorker OutputSettings {..} outputChan = do
  terminalCapabilities <- getTerminalCapabilitiesFromEnv
  let put chunks = do
        now <- getCurrentTime
        let timeChunk = fore yellow $ chunk $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" now
        putChunksWith terminalCapabilities $ timeChunk : " " : chunks
        putStrLn ""
  forever $ do
    event <- readChan outputChan
    case event of
      OutputEvent fsEvent -> do
        put
          [ fore cyan "event:   ",
            case fsEvent of
              Added {} -> fore green "added    "
              Modified {} -> fore yellow "modified "
              Removed {} -> fore red "removed  "
              Unknown {} -> "unknown  ",
            chunk $ T.pack $ eventPath fsEvent
          ]
      OutputKilling -> put [fore cyan "killing"]
      OutputKilled -> put [fore cyan "killed"]
      OutputProcessStarted command -> do
        case outputSettingClear of
          ClearScreen -> putStrLn "\ESCc"
          DoNotClearScreen -> pure ()
        let commandString = case command of
              [] -> ""
              (bin : args) -> showCommandForUser bin args
        put
          [ fore cyan "started:",
            " ",
            fore blue $ chunk $ T.pack commandString
          ]
      OutputProcessExited ec ->
        case ec of
          ExitSuccess ->
            put
              [ fore cyan "exited: ",
                " ",
                fore green "success"
              ]
          ExitFailure c ->
            put
              [ fore cyan "exited: ",
                " ",
                fore red $ chunk $ T.pack $ "failed: " <> show c
              ]
