{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Loop.OptParse where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Feedback.Common.OptParse
import Feedback.Common.Output
import System.Exit
import Text.Colour
import Text.Colour.Layout
#ifdef MIN_VERSION_safe_coloured_text_terminfo
import Text.Colour.Term (putChunksLocale)
#endif
import Text.Show.Pretty (pPrint)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO LoopSettings
combineToSettings flags@Flags {..} environment mConf = do
  let loops = maybe M.empty configLoops mConf
  mLoopConfig <- case flagCommand of
    "" -> pure Nothing
    _ ->
      Just <$> case M.lookup flagCommand loops of
        Nothing -> do
          when (not (null loops)) $
            putStrLn $
              unwords
                [ "No loop found with name",
                  show flagCommand <> ",",
                  "interpreting it as a standalone command."
                ]
          pure $ makeLoopConfiguration $ CommandScript flagCommand
        Just config -> do
          putStrLn $
            unwords
              [ "Interpreting",
                show flagCommand,
                "as the name of a configured loop."
              ]
          pure config
  case mLoopConfig of
    Nothing -> do
      put $ concatMap (<> ["\n"]) $ prettyConfiguration mConf
      exitSuccess
    Just loopConfig -> do
      loopSets <-
        combineToLoopSettings
          flags
          environment
          (mConf >>= configOutputConfiguration)
          loopConfig
      when (outputFlagDebug flagOutputFlags) $ pPrint loopSets
      pure loopSets
  where

#ifdef MIN_VERSION_safe_coloured_text_terminfo
    put = putChunksLocale
#else
    put = putChunksLocaleWith WithoutColours
#endif

prettyConfiguration :: Maybe Configuration -> [[Chunk]]
prettyConfiguration mConf = case mConf of
  Nothing -> [[fore blue "No feedback loops have been configured here."]]
  Just conf ->
    [ [fore blue "The following feedback loops are available:"],
      [""],
      layoutAsTable
        ( map
            (uncurry loopConfigLine)
            (M.toList (configLoops conf))
        ),
      [fore blue "Run ", fore yellow "feedback loopname", fore blue " to activate a feedback loop."]
    ]

loopConfigLine :: String -> LoopConfiguration -> [Chunk]
loopConfigLine loopName LoopConfiguration {..} =
  [ loopNameChunk $ loopName <> ":",
    maybe "no description" (chunk . T.pack) loopConfigDescription
  ]
