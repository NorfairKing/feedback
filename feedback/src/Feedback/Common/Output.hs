{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Feedback.Common.Output where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Word
import Feedback.Common.OptParse
import Path
import System.Exit
import Text.Colour
import Text.Printf

putTimedChunks :: TerminalCapabilities -> ZonedTime -> [Chunk] -> IO ()
putTimedChunks terminalCapabilities loopBegin chunks = do
  now <- getZonedTime
  let timeChunk = fore yellow $ chunk $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" now

  let relativeTimeStr :: NominalDiffTime -> String
      relativeTimeStr ndt =
        let d = realToFrac ndt :: Double
         in printf "%6.2fs" d
  let relativeTimeChunk = fore cyan $ chunk $ T.pack $ relativeTimeStr $ diffUTCTime (zonedTimeToUTC now) (zonedTimeToUTC loopBegin)
  putChunksLocaleWith terminalCapabilities $ timeChunk : " " : relativeTimeChunk : " " : chunks ++ ["\n"]

putDone :: TerminalCapabilities -> ZonedTime -> IO ()
putDone terminalCapabilities loopBegin = putTimedChunks terminalCapabilities loopBegin [indicatorChunk "done."]

indicatorChunk :: String -> Chunk
indicatorChunk = fore cyan . chunk . T.pack . printf "%-12s"

loopNameChunk :: String -> Chunk
loopNameChunk = fore yellow . chunk . T.pack

commandChunk :: String -> Chunk
commandChunk = fore blue . chunk . T.pack

startingLines :: RunSettings -> [[Chunk]]
startingLines RunSettings {..} =
  let RunSettings _ _ _ = undefined
   in concat
        [ case runSettingCommand of
            CommandArgs command ->
              [ [ indicatorChunk "starting",
                  " ",
                  commandChunk command
                ]
              ]
            CommandScript script ->
              [ [ indicatorChunk "starting script\n",
                  chunk $ T.pack script
                ]
              ],
          [ [ indicatorChunk "working dir:",
              " ",
              chunk $ T.pack $ fromAbsDir workdir
            ]
            | workdir <- maybeToList runSettingWorkingDir
          ],
          if null runSettingExtraEnv
            then []
            else
              [indicatorChunk "extra env:"] :
              map
                ( \(k, v) ->
                    [ "  ",
                      fore blue $ chunk (T.pack k),
                      ": ",
                      fore blue $ chunk (T.pack v)
                    ]
                )
                (M.toList runSettingExtraEnv)
        ]

exitCodeChunks :: ExitCode -> [Chunk]
exitCodeChunks ec =
  [ indicatorChunk "exited:",
    " ",
    case ec of
      ExitSuccess ->
        fore green "success"
      ExitFailure c ->
        fore red $ chunk $ T.pack $ "failed: " <> show c
  ]

durationChunks :: Word64 -> [Chunk]
durationChunks nanosecs =
  let diffTime :: Double
      diffTime = fromIntegral nanosecs / 1_000_000_000
   in [ indicatorChunk "took",
        " ",
        chunk $
          T.pack $ printf "%4.2fs" diffTime
      ]
