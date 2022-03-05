{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback where

import Brick.BChan
import Data.List
import Feedback.OptParse
import Feedback.TUI
import Feedback.TUI.Env
import Path
import Path.IO
import System.FSNotify as FS

runFeedbackMain :: IO ()
runFeedbackMain = do
  Settings {..} <- getSettings
  requestChan <- newBChan 1000
  here <- getCurrentDir
  let conf = FS.defaultConfig {confDebounce = Debounce 0.1}
  FS.withManagerConf conf $ \watchManager -> do
    stopListeningAction <-
      FS.watchTree
        watchManager
        (fromAbsDir here) -- Where to watch
        (eventFilter here)
        $ \event -> do
          writeBChan requestChan $ ReceiveEvent event
    feedbackTUI settingCommand requestChan
    stopListeningAction

eventFilter :: Path Abs Dir -> FS.Event -> Bool
eventFilter here fsEvent =
  and
    [ -- It's not one of those files that vim makes
      (filename <$> parseAbsFile (eventPath fsEvent)) /= Just [relfile|4913|],
      not $ "~" `isSuffixOf` eventPath fsEvent,
      -- It's not a hidden file
      not $ hiddenHere here (eventPath fsEvent)
    ]

hiddenHere :: Path Abs Dir -> FilePath -> Bool
hiddenHere here filePath =
  (hidden <$> (parseAbsFile filePath >>= stripProperPrefix here)) /= Just False

hidden :: Path Rel File -> Bool
hidden = goFile
  where
    goFile :: Path Rel File -> Bool
    goFile f = isHiddenIn (parent f) f || goDir (parent f)
    goDir :: Path Rel Dir -> Bool
    goDir f
      | parent f == f = False
      | otherwise = isHiddenIn (parent f) f || goDir (parent f)

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp
