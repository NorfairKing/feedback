{-# LANGUAGE RecordWildCards #-}

module Feedback.Test.OptParse where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Feedback.Common.OptParse
import Text.Show.Pretty (pPrint)

getSettings :: IO TestSettings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToTestSettings flags env config

data TestSettings = TestSettings
  { testSettingLoops :: !(Map String LoopSettings)
  }
  deriving (Show)

combineToTestSettings :: Flags -> Environment -> Maybe Configuration -> IO TestSettings
combineToTestSettings flags@Flags {..} environment mConf = do
  let filterFunc = case flagCommand of
        "" -> id
        _ -> M.filterWithKey (\k _ -> k == flagCommand)
  testSettingLoops <-
    traverse
      (combineToLoopSettings flags environment (mConf >>= configOutputConfiguration))
      (filterFunc $ maybe M.empty configLoops mConf)
  let testSets = TestSettings {..}
  when (outputFlagDebug flagOutputFlags) $ pPrint testSets
  pure testSets
