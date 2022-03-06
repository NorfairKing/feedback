{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback.Common.Output where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Word
import Feedback.Common.OptParse
import System.Exit
import Text.Colour
import Text.Printf

putTimedChunks :: TerminalCapabilities -> [Chunk] -> IO ()
putTimedChunks terminalCapabilities chunks = do
  now <- getCurrentTime
  let timeChunk = fore yellow $ chunk $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" now
  putChunksWith terminalCapabilities $ timeChunk : " " : chunks
  putStrLn ""

indicatorChunk :: String -> Chunk
indicatorChunk = fore cyan . chunk . T.pack . printf "%-8s"

loopNameChunk :: String -> Chunk
loopNameChunk = fore yellow . chunk . T.pack

commandChunk :: String -> Chunk
commandChunk = fore blue . chunk . T.pack

startingLines :: RunSettings -> [[Chunk]]
startingLines RunSettings {..} =
  concat
    [ [ [ indicatorChunk "starting",
          " ",
          commandChunk runSettingCommand
        ]
      ],
      [ [ indicatorChunk "extra env",
          " ",
          chunk $ T.pack $ show $ M.toList runSettingExtraEnv
        ]
        | not (null runSettingExtraEnv)
      ]
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
