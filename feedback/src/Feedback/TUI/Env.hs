module Feedback.TUI.Env where

import Brick.BChan
import Control.Monad.Reader
import Data.ByteString
import System.Exit
import System.FSNotify as FS
import System.Process.Typed as Typed
import UnliftIO

data Env = Env
  { envCommand :: ![String],
    envCurrentProcess :: !(MVar ProcessHandle),
    envRequestChan :: !(BChan Request),
    envResponseChan :: !(BChan Response)
  }

data ProcessHandle = ProcessHandle
  { processHandleProcess :: !P,
    processHandleWaiter :: Async ()
  }

type P = Typed.Process () Handle Handle

type W = ReaderT Env IO

data Request = ReceiveEvent !FS.Event

data Response
  = ReceivedEvent !FS.Event
  | ProcessStarted
  | ProcessExited !ExitCode
  | StdoutChunk !ByteString
  | StderrChunk !ByteString
