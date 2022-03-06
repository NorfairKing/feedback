{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Test where

import Control.Monad
import qualified Data.Map as M
import Feedback.Common.OptParse
import Feedback.Common.Output
import Feedback.Common.Process
import Feedback.Test.OptParse
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)

runFeedbackTest :: IO ()
runFeedbackTest = do
  TestSettings {..} <- getSettings
  terminalCapabilities <- getTerminalCapabilitiesFromEnv
  let put = putTimedChunks terminalCapabilities
  forM_ (M.toList testSettingLoops) $ \(loopName, LoopSettings {..}) -> do
    put [indicatorChunk "testing ", " ", loopNameChunk loopName]
    put [indicatorChunk "starting", " ", commandChunk loopSettingCommand]
    ec <- startProcessAndWait loopSettingCommand
    put $ exitCodeChunks ec
