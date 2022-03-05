module Feedback.TUI.State where

import System.Exit
import System.FSNotify as FS

data State = State
  { stateCommand :: [String],
    stateCurrentProcess :: !(Maybe ExitCode),
    stateEvents :: [FS.Event]
  }
  deriving (Show)

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)
