module Feedback.TUI.State where

import System.FSNotify as FS

data State = State
  { stateCommand :: [String],
    stateEvents :: [FS.Event]
  }
  deriving (Show)

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)
