module Feedback.TUI.State where

import Data.ByteString (ByteString)
import Data.Map
import qualified Data.Map as M
import Data.Word
import System.Exit
import System.FSNotify as FS

data State = State
  { stateCommand :: [String],
    stateCurrentProcess :: !(Maybe ExitCode),
    stateEvents :: [FS.Event],
    stateOutput :: !(NonEmptyCursor ByteString)
  }

addOutput :: OutputStream -> Word64 -> ByteString -> Output -> Output
addOutput os u bs (Output m) = Output $ M.insert u (os, bs) m

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)
