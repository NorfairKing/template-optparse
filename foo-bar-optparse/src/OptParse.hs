{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | = Optparse Template
--
-- This is a template implementation of commands, flags, options, environment variable and configuration file parsing according to best practices.
-- To use this template, follow the instructions below and delete anything you do not need.
--
-- == License
--
-- This template is __not__ free to use.
--
-- See https://template.cs-syd.eu/template/NorfairKing/template-optparse for more information.
--
-- Copyright (c) 2020-2022 Tom Sydney Kerckhove.
--
-- All Rights Reserved.
--
-- == Instructions
--
-- === Purpose
--
-- The purpose of this module is to determine everything that is needed for the workings of your program.
-- This includes dealing with command-line arguments, flags and options, environment variables and configuration files.
-- This information is gathered in an 'Instructions' type, which you can get using the `getInstructions` function.
-- After this information is gathered, your program should never have to consult the arguments, environment variables or configuration file anymore.
--
-- === Glossary
--
-- In what follows, I will use this terminology:
--
-- * Commands, Flags, Options: Whatever you put after the command on the command-line.
-- * Environment: Environment variables
-- * Configuration: Whatever you find in the configuration file
--
-- For the purposes of this code, we use the following types:
--
-- * 'Command': A sum type for the commands, each with their own flags and options.
-- * 'Flags': A product type for the flags and options that are common across commands.
-- * 'Arguments': The 'Command' and 'Flags' together.
-- * 'Environment': A product type for the relevant environment variables.
-- * 'Configuration': A product type for the relevant parts of the configuration file.
-- * 'Dispatch': A sum type for the commands, each with their own settings.
-- * 'Settings': A product type for the settings that are common across commands.
-- * 'Instructions': The 'Dispatch' and 'Settings' together.
--
-- === High-level workings
--
-- The gathering of the 'Instructions' happens in two stages.
--
-- In the first stage, the 'Arguments', 'Environment' and 'Configuration' values are gathered from their respective places.
-- At this point, these types should represent what is found, without any processing or validation.
--
-- In the second stage, in the `combineToInstructions` function, all these variables are combined together to produce the 'Instructions'.
-- It is in this phase that we do processing, validation and even 'System.Exit.die' if necessary.
--
-- === Included Example
--
-- This template comes with an example implementation for the 'OptParse' module for a hello world program that has one command: greet.
-- This command accepts a '--greeting' option, a 'FOO_BAR_GREETING' environment variable, or a 'greeting' field in the configuration file, to specify what to say when greeting the user.
-- The program also accepts  a '--polite' flag, a 'FOO_BAR_POLITE' environment variable, or a 'polite' field in the configuration file, to specify whether or not to be polite when greeting the user.
-- The greeting setting works for the 'greet' command only while the politeness setting works across commands.
--
-- === 'FilePath' Example
--
-- As an example, suppose our program uses a cache file for its 'compute' command.
--
-- 1. Have a constructor for the 'compute' command in the 'Command' sum type: 'CommandCompute'
-- 2. Add a 'ComputeArgs' type that contains a 'commandFlagCacheFile :: Maybe FilePath' field to indicate that the user may specify this file on the command-line.
-- 3. Add a field to the 'Environment' type to indicate that the user may specify this file in an environment variable.
-- 4. Add a field to the 'Configuration' type to indicate that the user may specify this file in the configuration file as well.
-- 5. Have a constructor for the 'compute' dispatch in the 'Dispatch' sum type: 'DispatchCompute'.
-- 6. Add a 'ComputeSettings' type that contains a 'computeSettingCacheFile :: Path Abs File' field that your program will use.
-- 7. Combine the all three in the 'combineToInstructions' function.
--
--
-- === Further instructions and support
--
-- Continue reading in the code inline for more detailed instructions.
--
-- If you have any trouble, you can contact @syd@ at @cs-syd@ dot @eu@ for support.
module OptParse
  ( -- * Interface
    getInstructions,
    Instructions (..),
    Dispatch (..),
    GreetSettings (..),
    Settings (..),

    -- ** Exposed for testing
    combineToInstructions,
    getArguments,
    prefs_,
    argParser,
    parseArgs,
    parseCommand,
    parseCommandGreet,
    parseFlags,
    Arguments (..),
    Command (..),
    GreetArgs (..),
    Flags (..),
    getEnvironment,
    environmentParser,
    Environment (..),
    getConfiguration,
    defaultConfigFile,
    Configuration (..),
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- | A product type for the settings that are common across commands
data Settings = Settings
  { settingPolite :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchGreet !GreetSettings
  deriving (Show, Eq, Generic)

-- | One type per command for its settings.
-- You can omit this if the command does not need specific settings.
data GreetSettings = GreetSettings
  { greetSettingGreeting :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

-- | Combine everything to instructions
combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  -- This is a typical way to combine a setting.
  --
  -- We choose the first of the supplied flag, environment variable or configuration field,
  -- or default value if none of the those were supplied.
  let settingPolite = fromMaybe True $ flagPolite <|> envPolite <|> mc configPolite
  let sets = Settings {..}
  disp <-
    -- Resolve the command-specific settings for each command
    case cmd of
      CommandGreet GreetArgs {..} -> do
        let greetSettingGreeting = greetArgGreeting <|> envGreeting <|> mc configGreeting
        pure $ DispatchGreet GreetSettings {..}
  pure $ Instructions disp sets
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the configuration file.
-- For example, use 'Maybe FilePath', not 'Path Abs File'.
--
-- Use 'readYamlConfigFile' or 'readFirstYamlConfigFile' to read a configuration.
data Configuration = Configuration
  { configPolite :: !(Maybe Bool),
    configGreeting :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

-- | We use @autodocodec@ for parsing a YAML config.
instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "polite" "Whether to be polite"
          .= configPolite
        <*> optionalField "greeting" "What to say when greeting"
          .= configGreeting

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

-- | Where to get the configuration file by default.
--
-- This uses the XDG base directory specifictation:
-- https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|optparse-template|])
  resolveFile xdgConfigDir "config.yaml"

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPolite :: !(Maybe Bool),
    envGreeting :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FOO_BAR_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var Env.auto "POLITE" (Env.help "Whether to be polite"))
      <*> optional (Env.var Env.str "GREETING" (Env.help "What to say when greeting"))

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandGreet !GreetArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "greet" $ CommandGreet <$> parseCommandGreet
      ]

-- | One type per command, for the command-specific arguments
data GreetArgs = GreetArgs
  { greetArgGreeting :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

-- | One 'optparse-applicative' parser for each command's flags
parseCommandGreet :: OptParse.ParserInfo GreetArgs
parseCommandGreet = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Greet the user"
    parser =
      GreetArgs
        <$> optional
          ( strOption
              ( mconcat
                  [ long "greeting",
                    help "What to say when greeting",
                    metavar "GREETING"
                  ]
              )
          )

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPolite :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
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
    <*> optional
      ( switch
          ( mconcat
              [ long "polite",
                help "Whether to be polite"
              ]
          )
      )
