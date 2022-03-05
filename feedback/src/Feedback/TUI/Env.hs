module Feedback.TUI.Env where

import Control.Monad.Reader
import System.FSNotify as FS

data Env = Env
  {
  }

type W = ReaderT Env IO

data Request = ReceiveEvent FS.Event

data Response = ReceivedEvent FS.Event
