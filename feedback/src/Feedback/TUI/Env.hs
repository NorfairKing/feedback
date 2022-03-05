module Feedback.TUI.Env where

import Brick.BChan
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader
import System.Exit
import System.FSNotify as FS
import System.Process.Typed as Typed

data Env = Env
  { envCommand :: ![String],
    envCurrentProcess :: !(MVar ProcessHandle),
    envRequestChan :: !(BChan Request),
    envResponseChan :: !(BChan Response)
  }

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !(Typed.Process () () ()),
    processHandleWaiter :: Async ()
  }

type W = ReaderT Env IO

data Request = ReceiveEvent !FS.Event

data Response
  = ReceivedEvent !FS.Event
  | ProcessStarted
  | ProcessExited !ExitCode
