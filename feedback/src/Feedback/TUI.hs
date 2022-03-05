{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.TUI where

import Brick.BChan
import Brick.Main
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader
import Feedback.TUI.Draw
import Feedback.TUI.Env
import Feedback.TUI.Handle
import Feedback.TUI.State
import Feedback.TUI.Worker
import Graphics.Vty (defaultConfig, mkVty)

feedbackTUI :: [String] -> BChan Request -> IO ()
feedbackTUI command requestChan = do
  -- Define the tui thread
  initialState <- buildInitialState command
  responseChan <- newBChan 1000
  let vtyBuilder = mkVty defaultConfig
  firstVty <- vtyBuilder
  let runTui = customMain firstVty vtyBuilder (Just responseChan) (tuiApp requestChan) initialState
  -- Define the worker thread
  envCurrentProcess <- newEmptyMVar
  let envCommand = command
  let envRequestChan = requestChan
  let envResponseChan = responseChan
  let env = Env {..}
  let runWorker = runReaderT tuiWorker env
  -- Left always works because the worker runs forever
  Left _ <- race runTui runWorker
  pure ()

tuiApp :: BChan Request -> App State Response ResourceName
tuiApp chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent chan,
      appStartEvent = pure,
      appAttrMap = buildAttrMap
    }

buildInitialState :: [String] -> IO State
buildInitialState stateCommand = do
  let stateEvents = []
  let stateCurrentProcess = Nothing
  let stateOutput = emptyOutput
  pure State {..}
