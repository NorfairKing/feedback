{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Feedback.Common.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (pretty)
import Path
import Path.IO
import Paths_feedback

data LoopSettings = LoopSettings
  { loopSettingRunSettings :: !RunSettings,
    loopSettingFilterSettings :: !FilterSettings,
    loopSettingOutputSettings :: !OutputSettings,
    loopSettingHooksSettings :: !HooksSettings
  }
  deriving (Show)

combineToLoopSettings :: Flags -> Environment -> Maybe OutputConfiguration -> LoopConfiguration -> IO LoopSettings
combineToLoopSettings Flags {..} Environment {} mDefaultOutputConfig LoopConfiguration {..} = do
  loopSettingRunSettings <- combineToRunSettings loopConfigRunConfiguration
  let loopSettingFilterSettings = combineToFilterSettings loopConfigFilterConfiguration

  let outputConfig = maybe loopConfigOutputConfiguration (<> loopConfigOutputConfiguration) mDefaultOutputConfig
  let loopSettingOutputSettings = combineToOutputSettings flagOutputFlags outputConfig
  loopSettingHooksSettings <- combineToHooksSettings loopConfigHooksConfiguration
  pure LoopSettings {..}

data RunSettings = RunSettings
  { runSettingCommand :: !Command,
    runSettingExtraEnv :: !(Map String String),
    runSettingWorkingDir :: !(Maybe (Path Abs Dir))
  }
  deriving (Show)

combineToRunSettings :: RunConfiguration -> IO RunSettings
combineToRunSettings RunConfiguration {..} = do
  let runSettingCommand = runConfigCommand
  let runSettingExtraEnv = runConfigExtraEnv
  runSettingWorkingDir <- mapM resolveDir' runConfigWorkingDir
  pure RunSettings {..}

data FilterSettings = FilterSettings
  { filterSettingGitignore :: !Bool,
    filterSettingFind :: !(Maybe String)
  }
  deriving (Show)

combineToFilterSettings :: FilterConfiguration -> FilterSettings
combineToFilterSettings FilterConfiguration {..} =
  let filterSettingGitignore = fromMaybe True filterConfigGitignore
      filterSettingFind = filterConfigFind
   in FilterSettings {..}

data OutputSettings = OutputSettings
  { outputSettingClear :: !Clear
  }
  deriving (Show)

combineToOutputSettings :: OutputFlags -> OutputConfiguration -> OutputSettings
combineToOutputSettings OutputFlags {..} mConf =
  let outputSettingClear =
        fromMaybe (if outputFlagDebug then DoNotClearScreen else ClearScreen) $
          outputFlagClear <|> outputConfigClear mConf
   in OutputSettings {..}

data HooksSettings = HooksSettings
  { hooksSettingBeforeAll :: Maybe RunSettings,
    hooksSettingAfterFirst :: Maybe RunSettings
  }
  deriving (Show)

combineToHooksSettings :: HooksConfiguration -> IO HooksSettings
combineToHooksSettings HooksConfiguration {..} = do
  hooksSettingBeforeAll <- mapM combineToRunSettings hooksConfigurationBeforeAll
  hooksSettingAfterFirst <- mapM combineToRunSettings hooksConfigurationAfterFirst
  pure HooksSettings {..}

data Configuration = Configuration
  { configLoops :: !(Map String LoopConfiguration),
    configOutputConfiguration :: !(Maybe OutputConfiguration)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldWithOmittedDefault' "loops" M.empty
          .= configLoops
        <*> optionalField "output" "default output configuration"
          .= configOutputConfiguration

emptyConfiguration :: Configuration
emptyConfiguration =
  Configuration
    { configLoops = mempty,
      configOutputConfiguration = mempty
    }

data LoopConfiguration = LoopConfiguration
  { loopConfigDescription :: !(Maybe String),
    loopConfigRunConfiguration :: !RunConfiguration,
    loopConfigFilterConfiguration :: !FilterConfiguration,
    loopConfigOutputConfiguration :: !OutputConfiguration,
    loopConfigHooksConfiguration :: !HooksConfiguration
  }
  deriving (Eq)

instance HasCodec LoopConfiguration where
  codec =
    named
      "LoopConfiguration"
      ( dimapCodec f g $
          eitherCodec (codec <?> "A bare command without any extra configuration") $
            object "LoopConfiguration" loopConfigurationObjectCodec
      )
      <??> loopConfigDocs
    where
      loopConfigDocs =
        [ "A LoopConfiguration specifies an entire feedback loop.",
          "",
          "It consists of four parts:",
          "* Filter Configuration: Which files to watch",
          "* Run Configuration: What to do when those files change",
          "* Output Configuration: What to see",
          "* Hooks configuration: What to around commands"
        ]
      f = \case
        Left s -> makeLoopConfiguration (CommandScript s)
        Right loopConfig -> loopConfig
      g loopConfig =
        let runConfig = loopConfigRunConfiguration loopConfig
            c = runConfigCommand runConfig
         in case c of
              CommandScript cmd | loopConfig == makeLoopConfiguration c -> Left cmd
              _ -> Right loopConfig

loopConfigurationObjectCodec :: JSONObjectCodec LoopConfiguration
loopConfigurationObjectCodec =
  LoopConfiguration
    <$> optionalField "description" "description of when to use this feedback loop"
      .= loopConfigDescription
    <*> parseAlternative
      (requiredField "run" "run configuration for this loop")
      runConfigurationObjectCodec
      .= loopConfigRunConfiguration
    <*> parseAlternative
      (requiredField "filter" "filter configuration for this loop")
      filterConfigurationObjectCodec
      .= loopConfigFilterConfiguration
    <*> parseAlternative
      (requiredField "output" "output configuration for this loop")
      outputConfigurationObjectCodec
      .= loopConfigOutputConfiguration
    <*> parseAlternative
      (requiredField "hooks" "hooks configuration for this loop")
      hooksConfigurationObjectCodec
      .= loopConfigHooksConfiguration

makeLoopConfiguration :: Command -> LoopConfiguration
makeLoopConfiguration c =
  LoopConfiguration
    { loopConfigDescription = Nothing,
      loopConfigRunConfiguration = makeRunConfiguration c,
      loopConfigFilterConfiguration = emptyFilterConfiguration,
      loopConfigOutputConfiguration = emptyOutputConfiguration,
      loopConfigHooksConfiguration = emptyHooksConfiguration
    }

data RunConfiguration = RunConfiguration
  { runConfigCommand :: !Command,
    runConfigExtraEnv :: !(Map String String),
    runConfigWorkingDir :: !(Maybe FilePath)
  }
  deriving (Eq)

instance HasCodec RunConfiguration where
  codec =
    named "RunConfiguration" $
      dimapCodec f g $
        eitherCodec
          (codec <?> "A bare command without any extra configuration")
          (object "RunConfiguration" runConfigurationObjectCodec)
    where
      f = \case
        Left s -> makeRunConfiguration (CommandScript s)
        Right loopConfig -> loopConfig
      g runConfig =
        let c = runConfigCommand runConfig
         in case c of
              CommandScript cmd | runConfig == makeRunConfiguration c -> Left cmd
              _ -> Right runConfig

runConfigurationObjectCodec :: JSONObjectCodec RunConfiguration
runConfigurationObjectCodec =
  RunConfiguration
    <$> commandObjectCodec
      .= runConfigCommand
    <*> optionalFieldWithOmittedDefault "env" M.empty "extra environment variables to set"
      .= runConfigExtraEnv
    <*> optionalField "working-dir" "where the process will be run"
      .= runConfigWorkingDir

makeRunConfiguration :: Command -> RunConfiguration
makeRunConfiguration c =
  RunConfiguration
    { runConfigCommand = c,
      runConfigExtraEnv = M.empty,
      runConfigWorkingDir = Nothing
    }

data FilterConfiguration = FilterConfiguration
  { filterConfigGitignore :: !(Maybe Bool),
    filterConfigFind :: !(Maybe String)
  }
  deriving (Eq)

instance HasCodec FilterConfiguration where
  codec =
    named
      "FilterConfiguration"
      ( object "FilterConfiguration" filterConfigurationObjectCodec
      )
      <??> filterConfigurationDocs
    where
      filterConfigurationDocs =
        [ "By default, standard filters are applied and,",
          "if in a git repository, only files in the git repository are considered.",
          "If either 'git' or 'find' configuration are specified, only those are used."
        ]

filterConfigurationObjectCodec :: JSONObjectCodec FilterConfiguration
filterConfigurationObjectCodec =
  FilterConfiguration
    <$> optionalField "git" "whether to ignore files that are not in the git repo\nConcretely, this uses `git ls-files` to find files that are in the repo, so files that have been added but are also ignored by .gitignore will still be watched."
      .= filterConfigGitignore
    <*> optionalField "find" "arguments for the 'find' command to find files to be notified about"
      .= filterConfigFind

emptyFilterConfiguration :: FilterConfiguration
emptyFilterConfiguration =
  FilterConfiguration
    { filterConfigGitignore = Nothing,
      filterConfigFind = Nothing
    }

data OutputConfiguration = OutputConfiguration
  { outputConfigClear :: !(Maybe Clear)
  }
  deriving (Eq)

instance HasCodec OutputConfiguration where
  codec =
    named "OutputConfiguration" $
      object "OutputConfiguration" outputConfigurationObjectCodec

outputConfigurationObjectCodec :: JSONObjectCodec OutputConfiguration
outputConfigurationObjectCodec =
  OutputConfiguration
    <$> optionalField "clear" "whether to clear the screen runs"
      .= outputConfigClear

instance Semigroup OutputConfiguration where
  (<>) oc1 oc2 =
    OutputConfiguration
      { outputConfigClear = outputConfigClear oc1 <|> outputConfigClear oc2
      }

emptyOutputConfiguration :: OutputConfiguration
emptyOutputConfiguration =
  OutputConfiguration
    { outputConfigClear = Nothing
    }

data HooksConfiguration = HooksConfiguration
  { hooksConfigurationBeforeAll :: !(Maybe RunConfiguration),
    hooksConfigurationAfterFirst :: !(Maybe RunConfiguration)
  }
  deriving (Eq)

instance HasCodec HooksConfiguration where
  codec =
    named "HooksConfiguration" $
      object "HooksConfiguration" hooksConfigurationObjectCodec

hooksConfigurationObjectCodec :: JSONObjectCodec HooksConfiguration
hooksConfigurationObjectCodec =
  HooksConfiguration
    <$> optionalField "before-all" "The hook to run before the first run"
      .= hooksConfigurationBeforeAll
    <*> optionalField "after-first" "The hook to run after the first run"
      .= hooksConfigurationAfterFirst

emptyHooksConfiguration :: HooksConfiguration
emptyHooksConfiguration =
  HooksConfiguration
    { hooksConfigurationBeforeAll = Nothing,
      hooksConfigurationAfterFirst = Nothing
    }

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  fp <- case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile
    Just cf -> resolveFile' cf
  getConfigurationFromFile fp

getConfigurationFromFile :: Path Abs File -> IO (Maybe Configuration)
getConfigurationFromFile = readYamlConfigFile

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  here <- getCurrentDir
  resolveFile here "feedback.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath)
  }

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FEEDBACK_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.help "Config file")

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    ( mconcat
        [ OptParse.progDesc versionStr,
          OptParse.fullDesc,
          OptParse.footerDoc (Just $ OptParse.pretty footerStr)
        ]
    )
  where
    versionStr =
      "Version: " <> showVersion version
    footerStr =
      unlines
        [ Env.helpDoc 80 environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

data Flags = Flags
  { flagCommand :: !String,
    flagConfigFile :: !(Maybe FilePath),
    flagOutputFlags :: !OutputFlags
  }

data OutputFlags = OutputFlags
  { outputFlagClear :: !(Maybe Clear),
    outputFlagDebug :: Bool
  }

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> parseCommandFlags
    <*> optional
      ( strOption
          ( mconcat
              [ short 'c',
                long "config-file",
                help "Path to an alternative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> parseOutputFlags

parseCommandFlags :: OptParse.Parser String
parseCommandFlags =
  let commandArg =
        strArgument
          ( mconcat
              [ help "The command to run",
                metavar "COMMAND",
                completer (listIOCompleter defaultConfigFileCompleter)
              ]
          )
   in unwords <$> many commandArg

defaultConfigFileCompleter :: IO [String]
defaultConfigFileCompleter = do
  mConfig <- defaultConfigFile >>= getConfigurationFromFile
  pure $ M.keys (maybe [] configLoops mConfig)

parseOutputFlags :: OptParse.Parser OutputFlags
parseOutputFlags =
  OutputFlags
    <$> parseClearFlag
    <*> switch
      ( mconcat
          [ short 'd',
            long "debug",
            help "show debug information"
          ]
      )

newtype Command = CommandScript {unScript :: String}
  deriving (Show, Eq)

instance HasCodec Command where
  codec = dimapCodec CommandScript unScript codec

commandObjectCodec :: JSONObjectCodec Command
commandObjectCodec =
  parseAlternative
    (requiredField "script" "the script to run on change")
    (requiredField "command" "the command to run on change (alias for 'script' for backward compatibility)")

data Clear = ClearScreen | DoNotClearScreen
  deriving (Show, Eq)

instance HasCodec Clear where
  codec = dimapCodec f g codec
    where
      f True = ClearScreen
      f False = DoNotClearScreen
      g ClearScreen = True
      g DoNotClearScreen = False

parseClearFlag :: OptParse.Parser (Maybe Clear)
parseClearFlag =
  optional $
    flag'
      ClearScreen
      ( mconcat
          [ long "clear",
            help "clear the screen between feedback"
          ]
      )
      <|> flag'
        DoNotClearScreen
        ( mconcat
            [ long "no-clear",
              help "do not clear the screen between feedback"
            ]
        )
