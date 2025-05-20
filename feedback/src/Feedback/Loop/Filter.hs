{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
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

#ifdef MIN_VERSION_Win32
getMinTTY :: IO Bool
getMinTTY = withHandleToHANDLE stdin isMinTTYHandle
#else
getMinTTY :: IO Bool
getMinTTY = pure False
#endif

data Filter
  = FilterWatchlist !(Set (Path Abs File))
  | FilterPredicate !(Path Abs Dir -> Bool) !(Path Abs File -> Bool)

instance Semigroup Filter where
  f1 <> f2 = case (f1, f2) of
    (FilterWatchlist s1, FilterWatchlist s2) -> FilterWatchlist (S.intersection s1 s2)
    (FilterWatchlist s, FilterPredicate dirPredicate filePredicate) ->
      FilterWatchlist $ S.filter filePredicate $ S.filter (dirPredicate . parent) s
    (FilterPredicate dirPredicate filePredicate, FilterWatchlist s) ->
      FilterWatchlist $ S.filter filePredicate $ S.filter (dirPredicate . parent) s
    (FilterPredicate dirPredicate1 filePredicate1, FilterPredicate dirPredicate2 filePredicate2) ->
      FilterPredicate (\d -> dirPredicate1 d && dirPredicate2 d) (\f -> filePredicate1 f && filePredicate2 f)

instance Monoid Filter where
  mempty = FilterPredicate (const True) (const True)
  mappend = (<>)

fileSetFilter :: Set (Path Abs File) -> Filter
fileSetFilter = FilterWatchlist

mkCombinedFilter :: Path Abs Dir -> FilterSettings -> IO Filter
mkCombinedFilter here filterSettings =
  mconcat
    <$> sequence
      [ mkGitFilter here filterSettings,
        mkFindFilter here filterSettings,
        pure $ standardFilter here
      ]

mkStdinFilter :: Path Abs Dir -> IO Filter
mkStdinFilter here = maybe mempty fileSetFilter <$> getStdinFiles here

getStdinFiles :: Path Abs Dir -> IO (Maybe (Set (Path Abs File)))
getStdinFiles here = do
  isTerminal <- hIsTerminalDevice stdin
  isMinTTY <- getMinTTY
  if isTerminal || isMinTTY
    then pure Nothing
    else
      (Just <$> handleFileSet here stdin)
        `catch` (\(_ :: IOException) -> pure Nothing)

mkGitFilter :: Path Abs Dir -> FilterSettings -> IO Filter
mkGitFilter here FilterSettings {..} = do
  if filterSettingGitignore
    then do
      mGitFiles <- gitLsFiles here
      pure $ maybe mempty fileSetFilter mGitFiles
    else pure mempty

gitLsFiles :: Path Abs Dir -> IO (Maybe (Set (Path Abs File)))
gitLsFiles here = do
  -- If there is no git directory, we'll get a 'fatal' message on stderr.
  -- We don't need the user to see this, so we setStderr nullStream.
  let processConfig = setStderr nullStream $ shell "git ls-files -z"
  (ec, out) <- readProcessStdout processConfig
  set <- nullBytesFileSet here out
  pure $ case ec of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just set

nullBytesFileSet :: Path Abs Dir -> LB.ByteString -> IO (Set (Path Abs File))
nullBytesFileSet here lb =
  runConduit $
    CL.sourceList (LB8.split '\NUL' lb)
      .| C.map LB.toStrict
      .| fileSetBuilder here

mkFindFilter :: Path Abs Dir -> FilterSettings -> IO Filter
mkFindFilter here FilterSettings {..} = case filterSettingFind of
  Nothing -> pure mempty
  Just args -> fileSetFilter <$> filesFromFindArgs here args

filesFromFindArgs :: Path Abs Dir -> String -> IO (Set (Path Abs File))
filesFromFindArgs here args = do
  let processConfig = setStdout createPipe $ shell $ "find " <> args
  (ec, out) <- readProcessStdout processConfig
  set <- lineBytesFileSet here out
  case ec of
    ExitFailure _ -> die $ "Find failed: " <> show ec
    ExitSuccess -> pure set

lineBytesFileSet :: Path Abs Dir -> LB.ByteString -> IO (Set (Path Abs File))
lineBytesFileSet here lb =
  runConduit $
    CL.sourceList (LB8.lines lb)
      .| C.map LB.toStrict
      .| fileSetBuilder here

handleFileSet :: Path Abs Dir -> Handle -> IO (Set (Path Abs File))
handleFileSet here h =
  runConduit $
    C.sourceHandle h
      .| C.linesUnboundedAscii
      .| fileSetBuilder here

fileSetBuilder :: Path Abs Dir -> ConduitT ByteString Void IO (Set (Path Abs File))
fileSetBuilder here =
  C.concatMap TE.decodeUtf8'
    .| C.map T.unpack
    .| C.mapM (resolveFile here)
    .| C.foldMap S.singleton

standardFilter :: Path Abs Dir -> Filter
standardFilter here =
  FilterPredicate
    (not . isHiddenIn here)
    ( \f ->
        and
          [ not $ isHiddenIn here f,
            -- It's not one of those files that vim makes
            not $ "~" `isSuffixOf` fromAbsFile f,
            filename f /= [relfile|4913|]
          ]
    )

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp

watchBasedOnFilter :: Path Abs Dir -> WatchManager -> Chan FS.Event -> Filter -> IO StopListening
watchBasedOnFilter here watchManager eventChan = \case
  -- If we have a watchlist of all the, then we don't need to list any files.
  -- We just watch all the relevant directories.
  FilterWatchlist fileSet -> do
    let dirMap :: Map (Path Abs Dir) (Set (Path Rel File))
        dirMap =
          M.unionsWith S.union $
            map (\f -> M.singleton (parent f) (S.singleton (filename f))) $
              S.toList fileSet
        subdirPredicate :: Path Abs Dir -> Set (Path Rel File) -> Event -> Bool
        subdirPredicate subdir subFileSet e = fromMaybe False $ do
          absFile <- parseAbsFile $ eventPath e
          relFile <- stripProperPrefix subdir absFile
          pure $ S.member relFile subFileSet
        watchSubdir :: Path Abs Dir -> Set (Path Rel File) -> IO StopListening
        watchSubdir subdir subFileSet = do
          watchDirChan watchManager (fromAbsDir subdir) (subdirPredicate subdir subFileSet) eventChan
    mconcat <$> mapM (uncurry watchSubdir) (M.toList dirMap)
  -- If we use a predicate then we have no choice but to walk the directory tree.
  FilterPredicate dirPredicate filePredicate ->
    let descendHandler :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (WalkAction Abs)
        descendHandler dir subdirs _ =
          pure $
            WalkExclude $
              -- Don't descend into directories that are not in the filter, so we don't spend time listing them.
              filter (not . dirPredicate) $
                -- Don't descent into hidden directories
                filter (isHiddenIn dir) subdirs
        outputWriter :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO StopListening
        outputWriter dir _ _ = do
          let eventFilter fsEvent = maybe False filePredicate $ parseAbsFile (eventPath fsEvent)
          watchDirChan watchManager (fromAbsDir dir) eventFilter eventChan
     in walkDirAccum (Just descendHandler) outputWriter here
