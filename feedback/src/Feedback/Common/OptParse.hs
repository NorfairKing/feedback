{-# LANGUAGE DeriveGeneric #-}
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
import qualified Data.Text.Encoding as TE
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

data LoopSettings = LoopSettings
  { loopSettingCommand :: !String,
    loopSettingOutputSettings :: !OutputSettings
  }
  deriving (Show, Eq, Generic)

combineToLoopSettings :: Flags -> Environment -> Maybe OutputConfiguration -> LoopConfiguration -> IO LoopSettings
combineToLoopSettings Flags {..} Environment {..} mDefaultOutputConfig LoopConfiguration {..} = do
  let loopSettingCommand = loopConfigCommand
  let outputConfig = liftA2 (<>) loopConfigOutputConfiguration mDefaultOutputConfig
  let loopSettingOutputSettings = combineToOutputSettings flagOutputFlags outputConfig
  pure LoopSettings {..}

data OutputSettings = OutputSettings
  { outputSettingClear :: !Clear
  }
  deriving (Show, Eq, Generic)

combineToOutputSettings :: OutputFlags -> Maybe OutputConfiguration -> OutputSettings
combineToOutputSettings OutputFlags {..} mConf =
  let outputSettingClear = fromMaybe ClearScreen $ outputFlagClear <|> (mConf >>= outputConfigClear)
   in OutputSettings {..}

data Configuration = Configuration
  { configLoops :: !(Map String LoopConfiguration),
    configOutputConfiguration :: !(Maybe OutputConfiguration)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldWithOmittedDefault' "loops" M.empty .= configLoops
        <*> optionalField "output" "default output configuration" .= configOutputConfiguration

data LoopConfiguration = LoopConfiguration
  { loopConfigCommand :: !String,
    loopConfigOutputConfiguration :: !(Maybe OutputConfiguration)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoopConfiguration)

instance HasCodec LoopConfiguration where
  codec =
    dimapCodec f g $
      disjointEitherCodec codec $
        object "LoopConfiguration" $
          LoopConfiguration
            <$> requiredField "command" "the command to run on change" .= loopConfigCommand
            <*> optionalField "output" "output configuration for this loop" .= loopConfigOutputConfiguration
    where
      f = \case
        Left c -> makeLoopConfiguration c
        Right loopConfig -> loopConfig
      g loopConfig@(LoopConfiguration c mOutputConfig) = case mOutputConfig of
        Nothing -> Left c
        Just _ -> Right loopConfig

makeLoopConfiguration :: String -> LoopConfiguration
makeLoopConfiguration c =
  LoopConfiguration
    { loopConfigCommand = c,
      loopConfigOutputConfiguration = Nothing
    }

data OutputConfiguration = OutputConfiguration
  { outputConfigClear :: !(Maybe Clear)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec OutputConfiguration)

instance HasCodec OutputConfiguration where
  codec =
    object "OutputConfiguration" $
      OutputConfiguration
        <$> optionalField "clear" "whether to clear the screen runs" .= outputConfigClear

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

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  here <- getCurrentDir
  resolveFile here "feedback.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FEEDBACK_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
  where
    mE = Env.def Nothing <> Env.keep

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
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagCommand :: !String,
    flagOutputFlags :: !OutputFlags,
    flagDebug :: Bool
  }
  deriving (Show, Eq, Generic)

data OutputFlags = OutputFlags
  { outputFlagClear :: !(Maybe Clear)
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> ( unwords
            <$> many
              ( strArgument
                  ( mconcat
                      [ help "The command to run"
                      ]
                  )
              )
        )
    <*> parseOutputFlags
    <*> switch (mconcat [long "debug", help "show debug information"])

parseOutputFlags :: OptParse.Parser OutputFlags
parseOutputFlags =
  OutputFlags
    <$> parseClearFlag

data Clear = ClearScreen | DoNotClearScreen
  deriving (Show, Eq, Generic)

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
