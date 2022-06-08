{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feedback.Loop.Filter where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.List
import Data.Maybe
import Data.Set
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Feedback.Common.OptParse
import Path
import Path.IO
import System.Exit
import System.FSNotify as FS
import System.Process.Typed as Typed
#ifdef MIN_VERSION_Win32
import System.Win32.MinTTY (isMinTTYHandle)
import System.Win32.Types (withHandleToHANDLE)
#endif
import UnliftIO

getStdinFiles :: Path Abs Dir -> IO (Maybe (Set FilePath))
getStdinFiles here = do
  isTerminal <- hIsTerminalDevice stdin
  isMinTTY <- getMinTTY
  if isTerminal || isMinTTY
    then pure Nothing
    else
      (Just <$> handleFileSet here stdin)
        `catch` (\(_ :: IOException) -> pure Nothing)
  where

#ifdef MIN_VERSION_Win32
      getMinTTY = withHandleToHANDLE stdin isMinTTYHandle
#else
      getMinTTY = pure False
#endif

mkEventFilter :: Path Abs Dir -> Maybe (Set FilePath) -> FilterSettings -> IO (FS.Event -> Bool)
mkEventFilter here mStdinFiles FilterSettings {..} = do
  let mFilter mSet event = maybe True (eventPath event `S.member`) mSet
  let stdinFilter = mFilter mStdinFiles
  mFindFiles <- mapM (filesFromFindArgs here) filterSettingFind
  let findFilter = mFilter mFindFiles
  mGitFiles <-
    if filterSettingGitingore
      then gitLsFiles here
      else pure Nothing
  let gitFilter = mFilter mGitFiles
  let standardFilter = standardEventFilter here
  pure $
    if isJust mStdinFiles
      then stdinFilter
      else
        if isJust mFindFiles
          then findFilter
          else combineFilters [standardFilter, gitFilter]

combineFilters :: [FS.Event -> Bool] -> FS.Event -> Bool
combineFilters filters event = all ($ event) filters

gitLsFiles :: Path Abs Dir -> IO (Maybe (Set FilePath))
gitLsFiles here = do
  let processConfig = shell "git ls-files"
  (ec, out) <- readProcessStdout processConfig
  set <- bytesFileSet here out
  pure $ case ec of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just set

filesFromFindArgs :: Path Abs Dir -> String -> IO (Set FilePath)
filesFromFindArgs here args = do
  let processConfig = setStdout createPipe $ shell $ "find " <> args
  (ec, out) <- readProcessStdout processConfig
  set <- bytesFileSet here out
  case ec of
    ExitFailure _ -> die $ "Find failed: " <> show ec
    ExitSuccess -> pure set

bytesFileSet :: Path Abs Dir -> LB.ByteString -> IO (Set FilePath)
bytesFileSet here lb =
  runConduit $
    CL.sourceList (LB8.lines lb)
      .| C.map LB.toStrict
      .| fileSetBuilder here

handleFileSet :: Path Abs Dir -> Handle -> IO (Set FilePath)
handleFileSet here h =
  runConduit $
    C.sourceHandle h
      .| C.linesUnboundedAscii
      .| fileSetBuilder here

fileSetBuilder :: Path Abs Dir -> ConduitT ByteString Void IO (Set FilePath)
fileSetBuilder here =
  C.concatMap TE.decodeUtf8'
    .| C.map T.unpack
    .| C.mapM (resolveFile here)
    .| C.map fromAbsFile
    .| C.foldMap S.singleton

standardEventFilter :: Path Abs Dir -> FS.Event -> Bool
standardEventFilter here fsEvent =
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
