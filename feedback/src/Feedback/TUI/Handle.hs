module Feedback.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
-- import Control.Monad.IO.Class
import Feedback.TUI.Env
import Feedback.TUI.State
import Graphics.Vty.Input.Events

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent _requestChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    AppEvent resp -> case resp of
      ReceivedEvent fsEvent -> continue $ s {stateEvents = fsEvent : stateEvents s}
      ProcessStarted -> continue $ s {stateCurrentProcess = Nothing}
      ProcessExited ec -> continue $ s {stateCurrentProcess = Just ec}
      StdoutChunk _ -> continue s
      StderrChunk _ -> continue s
    _ -> continue s
