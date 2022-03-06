{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Loop.OptParse where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Feedback.Common.OptParse
import GHC.Generics (Generic)
import Text.Show.Pretty (pPrint)

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingCommand :: !String,
    settingOutputSettings :: !OutputSettings
  }
  deriving (Show, Eq, Generic)

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {} mConf = do
  let loops = maybe M.empty configLoops mConf
  let defaultOutputConfig = mConf >>= configOutputConfiguration
  (settingCommand, outputConfig) <- case M.lookup flagCommand loops of
    Nothing -> do
      when (not (null loops)) $
        putStrLn $
          unwords
            [ "No loop found with name",
              show flagCommand <> ",",
              "interpreting it as a standalone command."
            ]
      pure (flagCommand, defaultOutputConfig)
    Just LoopConfiguration {..} -> do
      putStrLn $
        unwords
          [ "Interpreting",
            show flagCommand,
            "as the name of a configured loop."
          ]
      pure (loopConfigCommand, liftA2 (<>) loopConfigOutputConfiguration defaultOutputConfig)
  let settingOutputSettings = combineToOutputSettings flagOutputFlags outputConfig
  let settings = Settings {..}
  when flagDebug $ pPrint settings
  pure settings
