{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Test where

import Control.Monad
import qualified Data.Map as M
import Feedback.Common.OptParse
import Feedback.Common.Output
import Feedback.Common.Process
import Feedback.Test.OptParse
import GHC.Clock (getMonotonicTimeNSec)

#ifdef MIN_VERSION_safe_coloured_text_terminfo
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)
#else
import Text.Colour.Capabilities (TerminalCapabilities(..))
#endif

runFeedbackTest :: IO ()
runFeedbackTest = do
  TestSettings {..} <- getSettings
  terminalCapabilities <- getTermCaps
  let put = putTimedChunks terminalCapabilities
  forM_ (M.toList testSettingLoops) $ \(loopName, LoopSettings {..}) -> do
    put [indicatorChunk "testing ", " ", loopNameChunk loopName]
    mapM_ put $ startingLines loopSettingRunSettings
    start <- getMonotonicTimeNSec
    ec <- startProcessAndWait loopSettingRunSettings
    end <- getMonotonicTimeNSec
    put $ exitCodeChunks ec
    let duration = end - start
    put $ durationChunks duration
  where

#ifdef MIN_VERSION_safe_coloured_text_terminfo
    getTermCaps = getTerminalCapabilitiesFromEnv
#else
    getTermCaps = WithoutColours
#endif
