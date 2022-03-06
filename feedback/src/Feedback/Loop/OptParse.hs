{-# LANGUAGE RecordWildCards #-}

module Feedback.Loop.OptParse where

import Control.Monad
import qualified Data.Map as M
import Feedback.Common.OptParse
import Text.Show.Pretty (pPrint)

getLoopSettings :: IO LoopSettings
getLoopSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO LoopSettings
combineToSettings flags@Flags {..} environment mConf = do
  let loops = maybe M.empty configLoops mConf
  loopConfig <- case M.lookup flagCommand loops of
    Nothing -> do
      when (not (null loops)) $
        putStrLn $
          unwords
            [ "No loop found with name",
              show flagCommand <> ",",
              "interpreting it as a standalone command."
            ]
      pure $ makeLoopConfiguration $ CommandArgs flagCommand
    Just config -> do
      putStrLn $
        unwords
          [ "Interpreting",
            show flagCommand,
            "as the name of a configured loop."
          ]
      pure config
  loopSets <-
    combineToLoopSettings
      flags
      environment
      (mConf >>= configOutputConfiguration)
      loopConfig
  when flagDebug $ pPrint loopSets
  pure loopSets
