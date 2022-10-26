{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Test where

import Control.Monad
import qualified Data.Map as M
import Data.Time
import Feedback.Common.OptParse
import Feedback.Common.Output
import Feedback.Common.Process
import Feedback.Test.OptParse
import GHC.Clock (getMonotonicTimeNSec)
import Text.Colour.Capabilities (TerminalCapabilities (..))
#ifdef MIN_VERSION_safe_coloured_text_terminfo
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
#endif

runFeedbackTest :: IO ()
runFeedbackTest = do
  TestSettings {..} <- getSettings
  terminalCapabilities <- getTermCaps
  begin <- getZonedTime
  let put = putTimedChunks terminalCapabilities begin
  forM_ (M.toList testSettingLoops) $ \(loopName, LoopSettings {..}) -> do
    put [indicatorChunk "testing ", " ", loopNameChunk loopName]
    mapM_ put $ startingLines loopSettingRunSettings
    start <- getMonotonicTimeNSec
    ec <- startProcessAndWait loopSettingRunSettings
    end <- getMonotonicTimeNSec
    put $ exitCodeChunks ec
    let duration = end - start
    put $ durationChunks duration

#ifdef MIN_VERSION_safe_coloured_text_terminfo
getTermCaps :: IO TerminalCapabilities
getTermCaps = getTerminalCapabilitiesFromEnv
#else
getTermCaps :: IO TerminalCapabilities
getTermCaps = pure WithoutColours
#endif
