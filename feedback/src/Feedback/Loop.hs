{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feedback.Loop where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (AsyncException (UserInterrupt))
import Control.Monad
import qualified Data.Text as T
import Data.Time
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
import System.Posix.Signals as Signal
import Text.Colour
import UnliftIO
#ifdef MIN_VERSION_Win32
import System.Win32.MinTTY (isMinTTYHandle)
import System.Win32.Types (withHandleToHANDLE)
#endif
#ifdef MIN_VERSION_safe_coloured_text_terminfo
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
#else
import Text.Colour.Capabilities (TerminalCapabilities(..))
#endif

runFeedbackLoop :: IO ()
runFeedbackLoop = do
  -- The outer loop happens here, before 'getLoopSettings'
  -- so that the loop can be the thing that's being worked on as well.
  here <- getCurrentDir

  -- We must get the stdin filter beforehand, because stdin can only be
  -- consumed once and we'll want to be able to reread filters below.
  stdinFilter <- mkStdinFilter here

  -- Figure out if colours are supported up front, no need to do that in the
  -- loop.
  terminalCapabilities <- getTermCaps

  -- Get the threadid for a child process to throw an exception to when it's
  -- being killed by the user.
  mainThreadId <- myThreadId

  -- Get the flags and the environment up front, because they don't change
  -- anyway.
  -- This is also important because autocompletion won't work if we output
  -- something before parsing the flags.
  flags <- getFlags
  env <- getEnvironment

  -- Make sure the user knows what's happening.
  -- Note that this this must occur after the getFlags so that nothing is
  -- output before the autocompletion options can be activated.
  -- Otherwise the autocompletion output will fail to parse and autocomplete
  -- breaks.
  firstBegin <- getZonedTime
  putTimedChunks terminalCapabilities firstBegin [indicatorChunk "preparing for first run"]

  -- Get the initial configuration so that we know if we need to run a hook after the first run.
  mInitialConfiguration <- getConfiguration flags env
  initialSettings <- combineToSettings flags env mInitialConfiguration

  -- If a before-all hook is defined, run it now.
  forM_ (hooksSettingBeforeAll (loopSettingHooksSettings initialSettings)) $ \beforeAllRunSettings -> do
    putTimedChunks terminalCapabilities firstBegin [indicatorChunk "starting before-all hook"]
    runHook terminalCapabilities firstBegin beforeAllRunSettings

  -- Define how to run a single loop with given settings in a let binding so we
  -- don't have to pass in args like 'here', 'stdinFilter' and
  -- 'terminalCapabilities'.
  let doSingleLoopWithSettings firstLoop loopBegin loopSettings = do
        FS.withManagerConf FS.defaultConfig $ \watchManager -> do
          -- Set up watchers for each relevant directory and send the FSNotify
          -- events down this event channel.
          eventChan <- newChan
          stopListeningAction <-
            startWatching
              here
              stdinFilter
              terminalCapabilities
              loopBegin
              loopSettings
              watchManager
              eventChan

          -- Start the process and put output.
          worker mainThreadId firstLoop loopSettings terminalCapabilities loopBegin eventChan stopListeningAction

  -- Do the first loop outside the 'forever' so we can run commands before and
  -- after the first loop.
  doSingleLoopWithSettings True firstBegin initialSettings
    `finally` putDone terminalCapabilities firstBegin

  let doSingleLoop loopBegin = do
        -- We show a 'preparing' chunk before we get the settings because sometimes
        -- getting the settings can take a while, for example in big repositories.
        putTimedChunks terminalCapabilities loopBegin [indicatorChunk "preparing"]

        -- Get the loop configuration within the loop, so that the loop
        -- configuration can be what is being worked on.
        mConfiguration <- getConfiguration flags env
        loopSettings <- combineToSettings flags env mConfiguration

        doSingleLoopWithSettings False loopBegin loopSettings

  let singleIteration = do
        -- Record when the loop began so we can show relative times nicely.
        loopBegin <- getZonedTime
        doSingleLoop loopBegin `finally` putDone terminalCapabilities loopBegin

  forever singleIteration

runHook :: TerminalCapabilities -> ZonedTime -> RunSettings -> IO ()
runHook terminalCapabilities begin runSettings = do
  mapM_ (putTimedChunks terminalCapabilities begin) (startingLines runSettings)
  _ <- startProcessHandle runSettings
  -- We want this process to keep running, so we don't wait for it.
  pure ()

startWatching ::
  Path Abs Dir ->
  Filter ->
  TerminalCapabilities ->
  ZonedTime ->
  LoopSettings ->
  WatchManager ->
  Chan FS.Event ->
  IO StopListening
startWatching here stdinFilter terminalCapabilities loopBegin LoopSettings {..} watchManager eventChan = do
  let sendOutput :: Output -> IO ()
      sendOutput = putOutput loopSettingOutputSettings terminalCapabilities loopBegin

  -- Build the filter that says which files and directories to care about
  sendOutput OutputFiltering
  f <- (stdinFilter <>) <$> mkCombinedFilter here loopSettingFilterSettings

  -- Set up the fsnotify watchers based on that filter
  sendOutput OutputWatching
  let descendHandler :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (WalkAction Abs)
      descendHandler dir subdirs _ =
        -- Don't descent into hidden directories
        pure $ WalkExclude $ filter (isHiddenIn dir) subdirs
      outputWriter :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO StopListening
      outputWriter dir _ _ =
        if filterDirFilter f dir
          then do
            let eventFilter fsEvent = maybe False (filterFileFilter f) $ parseAbsFile (eventPath fsEvent)
            watchDirChan watchManager (fromAbsDir dir) eventFilter eventChan
          else pure (pure ())
  walkDirAccum (Just descendHandler) outputWriter here

#ifdef MIN_VERSION_safe_coloured_text_terminfo
getTermCaps :: IO TerminalCapabilities
getTermCaps = getTerminalCapabilitiesFromEnv
#else
getTermCaps :: IO TerminalCapabilities
getTermCaps = pure WithoutColours
#endif

data RestartEvent
  = FSEvent !FS.Event
  | StdinEvent !Char
  deriving (Show, Eq)

worker ::
  ThreadId ->
  Bool ->
  LoopSettings ->
  TerminalCapabilities ->
  ZonedTime ->
  Chan FS.Event ->
  IO () ->
  IO ()
worker
  mainThreadId
  thisIsTheFirstLoop
  LoopSettings {..}
  terminalCapabilities
  loopBegin
  eventChan
  stopListeningAction = do
    let sendOutput :: Output -> IO ()
        sendOutput = putOutput loopSettingOutputSettings terminalCapabilities loopBegin

    -- Record starting time of the process.
    -- This is different from 'loopBegin' because preparing the watchers may take
    -- a nontrivial amount of time.
    start <- getMonotonicTimeNSec

    -- Start the process process
    processHandle <- startProcessHandle loopSettingRunSettings
    sendOutput $ OutputProcessStarted loopSettingRunSettings

    -- Perform GC after the process has started, because that's when we're
    -- waiting anyway, so that we don't need idle gc.
    performGC

    -- Make sure we kill the process and wait for it to exit if a user presses
    -- C-c.
    installKillHandler mainThreadId processHandle

    let runAfterFirstHookIfNecessary = do
          -- If this is the first run AND a after-first hook is defined, run it now.
          when thisIsTheFirstLoop $
            forM_ (hooksSettingAfterFirst loopSettingHooksSettings) $ \afterFirstRunSettings -> do
              putTimedChunks terminalCapabilities loopBegin [indicatorChunk "starting after-first hook"]
              runHook terminalCapabilities loopBegin afterFirstRunSettings
    -- From here on we will wait for either:
    -- 1. A change to a file that we are watching, or
    -- 2. The process to finish.

    -- 1. If An event happened first, output it and kill the process.
    let handleEventHappened event = do
          -- Output the event that has fired
          sendOutput $ OutputEvent event
          -- Output that we'll stop watching now
          sendOutput OutputStopWatching
          -- Stop watching
          stopListeningAction
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
          -- Process is done, run the after-first hook if necessary
          runAfterFirstHookIfNecessary

    -- 2. If the process finished first, show the result and wait for an event anyway
    let handleProcessDone ec = do
          end <- getMonotonicTimeNSec
          sendOutput $ OutputProcessExited ec (end - start)
          -- Process is done, run the after-first hook if necessary
          runAfterFirstHookIfNecessary
          -- Output the event that made the rerun happen
          event <- waitForEvent eventChan
          sendOutput $ OutputEvent event
          -- Output that we'll stop watching now
          sendOutput OutputStopWatching
          -- Stop watching
          stopListeningAction

    -- Either wait for it to finish or wait for an event
    eventOrDone <-
      race
        (waitForEvent eventChan)
        (waitProcessHandle processHandle)

    case eventOrDone of
      Left event -> handleEventHappened event
      Right ec -> handleProcessDone ec

installKillHandler :: ThreadId -> ProcessHandle -> IO ()
installKillHandler mainThreadId processHandle = do
  let killHandler :: Signal.Handler
      killHandler = CatchOnce $ do
        stopProcessHandle processHandle
        _ <- waitProcessHandle processHandle
        -- Throw a 'UserInterrupt' to the main thread so that the main thread
        -- can print done after the child processes have exited.
        throwTo mainThreadId UserInterrupt

  -- Install this kill handler for sigINT only.
  -- In the case of sigKILL, which we can't really be sure to catch anyway,
  -- crash harder.
  _ <- installHandler sigINT killHandler Nothing
  pure ()

waitForEvent :: Chan FS.Event -> IO RestartEvent
waitForEvent eventChan = do
  isTerminal <- hIsTerminalDevice stdin
  isMinTTY <- getMinTTY
  if isTerminal || isMinTTY
    then do
      hSetBuffering stdin NoBuffering
      either id id
        <$> race
          (StdinEvent <$> hGetChar stdin)
          (FSEvent <$> readChan eventChan)
    else FSEvent <$> readChan eventChan

data Output
  = OutputFiltering
  | OutputWatching
  | OutputStopWatching
  | OutputEvent !RestartEvent
  | OutputKilling
  | OutputKilled
  | OutputProcessStarted !RunSettings
  | OutputProcessExited !ExitCode !Word64
  deriving (Show)

putOutput :: OutputSettings -> TerminalCapabilities -> ZonedTime -> Output -> IO ()
putOutput OutputSettings {..} terminalCapabilities loopBegin =
  let put = putTimedChunks terminalCapabilities loopBegin
   in \case
        OutputFiltering -> put [indicatorChunk "filtering"]
        OutputWatching -> put [indicatorChunk "watching"]
        OutputStopWatching -> put [indicatorChunk "stop watching"]
        OutputEvent restartEvent -> do
          put $
            indicatorChunk "event:"
              : " "
              : case restartEvent of
                FSEvent fsEvent ->
                  [ case fsEvent of
                      Added {} -> fore green " added    "
                      Modified {} -> fore yellow " modified "
                      Removed {} -> fore red " removed  "
                      Unknown {} -> " unknown  "
                      ModifiedAttributes {} -> fore yellow " modified "
                      WatchedDirectoryRemoved {} -> fore red " removed "
                      CloseWrite {} -> fore yellow " modified ",
                    chunk $ T.pack $ eventPath fsEvent
                  ]
                StdinEvent c -> [fore magenta "manual restart: ", chunk $ T.pack $ show c]
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
