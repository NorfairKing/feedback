module Feedback.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad.IO.Class
import Feedback.TUI.Env
import Feedback.TUI.State
import GHC.Clock (getMonotonicTimeNSec)
import Graphics.Vty.Input.Events

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent _requestChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    AppEvent resp -> case resp of
      ReceivedEvent fsEvent ->
        continue $ s {stateEvents = fsEvent : stateEvents s}
      ProcessStarted ->
        continue $
          s
            { stateEvents = [],
              stateCurrentProcess = Nothing,
              stateOutput = emptyOutput
            }
      ProcessExited ec ->
        continue $ s {stateCurrentProcess = Just ec}
      StdoutChunk contents -> do
        now <- liftIO getMonotonicTimeNSec
        continue $ s {stateOutput = addOutput Stdout now contents (stateOutput s)}
      StderrChunk contents -> do
        now <- liftIO getMonotonicTimeNSec
        continue $ s {stateOutput = addOutput Stderr now contents (stateOutput s)}
    _ -> continue s
