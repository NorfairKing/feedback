{-# LANGUAGE RecordWildCards #-}

module Feedback.Test.OptParse where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Feedback.Common.OptParse
import GHC.Generics (Generic)
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
  deriving (Show, Eq)

combineToTestSettings :: Flags -> Environment -> Maybe Configuration -> IO TestSettings
combineToTestSettings flags@Flags {..} environment mConf = do
  testSettingLoops <-
    traverse
      (combineToLoopSettings flags environment (mConf >>= configOutputConfiguration))
      (maybe M.empty configLoops mConf)
  pure TestSettings {..}
