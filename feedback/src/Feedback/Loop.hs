{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feedback.Loop where

import Control.Monad
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Maybe
import Data.Set
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import Feedback.Common.OptParse
import Feedback.Common.Output
import Feedback.Common.Process
import Feedback.Loop.OptParse
import GHC.Clock (getMonotonicTimeNSec)
import Path
import Path.IO
import System.Exit
import System.FSNotify as FS
import System.Process.Typed as Typed
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

getStdinFiles :: Path Abs Dir -> IO (Maybe (Set FilePath))
getStdinFiles here = do
  isTerminal <- hIsTerminalDevice stdin
  if isTerminal
    then pure Nothing
    else
      (Just <$> handleFileSet here stdin)
        `catch` (\(_ :: IOException) -> pure Nothing)

mkEventFilter :: Path Abs Dir -> Maybe (Set FilePath) -> FilterSettings -> IO (FS.Event -> Bool)
mkEventFilter here mStdinFiles FilterSettings {..} = do
  let mFilter mSet event = maybe True (eventPath event `S.member`) mSet
  let stdinFilter = mFilter mStdinFiles
  mFindFiles <- mapM (filesFromFindArgs here) filterSettingFind
  let findFilter = mFilter mFindFiles
  mGitFiles <-
    if filterSettingGitingore
      then gitLsFiles here
      else pure Nothing
  let gitFilter = mFilter mGitFiles
  let standardFilter = standardEventFilter here
  pure $
    if isJust mStdinFiles
      then stdinFilter
      else
        if isJust mFindFiles
          then findFilter
          else combineFilters [standardFilter, gitFilter]

combineFilters :: [FS.Event -> Bool] -> FS.Event -> Bool
combineFilters filters event = all ($ event) filters

gitLsFiles :: Path Abs Dir -> IO (Maybe (Set FilePath))
gitLsFiles here = do
  let processConfig = setStdout createPipe $ shell "git ls-files"
  process <- startProcess processConfig
  ec <- waitExitCode process
  case ec of
    ExitFailure _ -> pure Nothing
    ExitSuccess -> Just <$> handleFileSet here (getStdout process)

filesFromFindArgs :: Path Abs Dir -> String -> IO (Set FilePath)
filesFromFindArgs here args = do
  let processConfig = setStdout createPipe $ shell $ "find " <> args
  process <- startProcess processConfig
  ec <- waitExitCode process
  case ec of
    ExitFailure _ -> die $ "Find failed: " <> show ec
    ExitSuccess -> handleFileSet here (getStdout process)

handleFileSet :: Path Abs Dir -> Handle -> IO (Set FilePath)
handleFileSet here h =
  runConduit $
    C.sourceHandle h
      .| C.linesUnboundedAscii
      .| C.concatMap TE.decodeUtf8'
      .| C.map T.unpack
      .| C.mapM (resolveFile here)
      .| C.map fromAbsFile
      .| C.foldMap S.singleton

standardEventFilter :: Path Abs Dir -> FS.Event -> Bool
standardEventFilter here fsEvent =
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

processWorker :: RunSettings -> Chan FS.Event -> Chan Output -> IO ()
processWorker runSettings eventChan outputChan = do
  let sendOutput = writeChan outputChan
  -- Record starting time
  start <- getMonotonicTimeNSec
  -- Start process
  processHandle <- startProcessHandle runSettings
  -- Output that the process has started
  sendOutput $ OutputProcessStarted runSettings
  -- Either wait for it to finish or wait for an event
  eventOrDone <- race (readChan eventChan) (waitProcessHandle processHandle)
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
      event <- readChan eventChan
      sendOutput $ OutputEvent event

data Output
  = OutputEvent !FS.Event
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
      OutputEvent fsEvent -> do
        put
          [ indicatorChunk "event:",
            case fsEvent of
              Added {} -> fore green " added    "
              Modified {} -> fore yellow " modified "
              Removed {} -> fore red " removed  "
              Unknown {} -> " unknown  ",
            chunk $ T.pack $ eventPath fsEvent
          ]
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
