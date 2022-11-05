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
import Data.Set
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Feedback.Common.OptParse
import Path
import Path.IO
import System.Exit
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

data Filter = Filter
  { filterDirFilter :: Path Abs Dir -> Bool,
    filterFileFilter :: Path Abs File -> Bool
  }

instance Semigroup Filter where
  f1 <> f2 =
    Filter
      { filterDirFilter = \d -> filterDirFilter f1 d && filterDirFilter f2 d,
        filterFileFilter = \f -> filterFileFilter f1 f && filterFileFilter f2 f
      }

instance Monoid Filter where
  mempty = Filter {filterDirFilter = const True, filterFileFilter = const True}
  mappend = (<>)

fileSetFilter :: Set (Path Abs File) -> Filter
fileSetFilter fileSet =
  let dirSet = S.map parent fileSet
   in Filter
        { filterDirFilter = (`S.member` dirSet),
          filterFileFilter = (`S.member` fileSet)
        }

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
  if filterSettingGitingore
    then do
      mGitFiles <- gitLsFiles here
      pure $ maybe mempty fileSetFilter mGitFiles
    else pure mempty

gitLsFiles :: Path Abs Dir -> IO (Maybe (Set (Path Abs File)))
gitLsFiles here = do
  let processConfig = shell "git ls-files"
  (ec, out) <- readProcessStdout processConfig
  set <- bytesFileSet here out
  pure $ case ec of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just set

mkFindFilter :: Path Abs Dir -> FilterSettings -> IO Filter
mkFindFilter here FilterSettings {..} = case filterSettingFind of
  Nothing -> pure mempty
  Just args -> fileSetFilter <$> filesFromFindArgs here args

filesFromFindArgs :: Path Abs Dir -> String -> IO (Set (Path Abs File))
filesFromFindArgs here args = do
  let processConfig = setStdout createPipe $ shell $ "find " <> args
  (ec, out) <- readProcessStdout processConfig
  set <- bytesFileSet here out
  case ec of
    ExitFailure _ -> die $ "Find failed: " <> show ec
    ExitSuccess -> pure set

bytesFileSet :: Path Abs Dir -> LB.ByteString -> IO (Set (Path Abs File))
bytesFileSet here lb =
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
  Filter
    { filterDirFilter = not . isHiddenIn here,
      filterFileFilter = \f ->
        and
          [ not $ isHiddenIn here f,
            -- It's not one of those files that vim makes
            not $ "~" `isSuffixOf` fromAbsFile f,
            filename f /= [relfile|4913|]
          ]
    }

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
