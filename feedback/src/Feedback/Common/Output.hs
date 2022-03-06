{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Feedback.Common.Output where

import qualified Data.Text as T
import Data.Time
import System.Exit
import Text.Colour

putTimedChunks :: TerminalCapabilities -> [Chunk] -> IO ()
putTimedChunks terminalCapabilities chunks = do
  now <- getCurrentTime
  let timeChunk = fore yellow $ chunk $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" now
  putChunksWith terminalCapabilities $ timeChunk : " " : chunks
  putStrLn ""

indicatorChunk :: String -> Chunk
indicatorChunk = fore cyan . chunk . T.pack

loopNameChunk :: String -> Chunk
loopNameChunk = chunk . T.pack

commandChunk :: String -> Chunk
commandChunk = fore blue . chunk . T.pack

exitCodeChunks :: ExitCode -> [Chunk]
exitCodeChunks = \case
  ExitSuccess ->
    [ fore cyan "exited: ",
      " ",
      fore green "success"
    ]
  ExitFailure c ->
    [ fore cyan "exited: ",
      " ",
      fore red $ chunk $ T.pack $ "failed: " <> show c
    ]
