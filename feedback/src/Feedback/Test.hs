{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Test where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Feedback.Common.OptParse
import Feedback.Common.Process
import Feedback.Test.OptParse
import System.Exit
import Text.Colour
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)

runFeedbackTest :: IO ()
runFeedbackTest = do
  TestSettings {..} <- getSettings
  terminalCapabilities <- getTerminalCapabilitiesFromEnv
  let put chunks = do
        now <- getCurrentTime
        let timeChunk = fore yellow $ chunk $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" now
        putChunksWith terminalCapabilities $ timeChunk : " " : chunks
        putStrLn ""

  forM_ (M.toList testSettingLoops) $ \(loopName, LoopSettings {..}) -> do
    put [fore cyan "testing ", " ", chunk $ T.pack loopName]
    put [fore cyan "starting", " ", fore blue $ chunk $ T.pack loopSettingCommand]
    ec <- startProcessAndWait loopSettingCommand
    case ec of
      ExitSuccess ->
        put
          [ fore cyan "exited: ",
            " ",
            fore green "success"
          ]
      ExitFailure c ->
        put
          [ fore cyan "exited: ",
            " ",
            fore red $ chunk $ T.pack $ "failed: " <> show c
          ]
