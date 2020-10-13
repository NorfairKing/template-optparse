{-# LANGUAGE DeriveGeneric #-}
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
-- Copyright (c) 2020 Tom Sydney Kerckhove.
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
-- * 'Settings': A product type for the settings that are command across commands.
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
-- === 'FilePath' Example
--
-- As an example, suppose our program uses a cache file for its 'compute' command.
--
-- 1. Hhave a constructor for the 'compute' command that in the 'Command' sum type: 'CommandCompute'
-- 2. Add a 'ComputeFlags' type that contains a 'commandFlagCacheFile :: Maybe FilePath' field to indicate that the user may specify this file on the command-line.
-- 3. Add a field to the 'Environment' type to indicate that the user may specify this file in an environment variable.
-- 4. Add a field to the 'Configuration' type to indicate that the user may specify this file in the configuration file as well.
-- 5. Combine the above three in the 'combineToInstructions' function.
--
--
-- === Further instructions and support
--
-- Continue reading in the code inline for more detailed instructions.
--
-- You can see an example of option parsing according to this template at https://github.com/NorfairKing/intray/blob/master/intray-cli/src/Intray/Cli/OptParse.hs
--
-- If you have any trouble, you can contact @syd@ at @cs-syd@ dot @eu@ for support.
module OptParse where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import qualified System.Environment as System
import YamlParse.Applicative as YamlParse

-- The combination of the command with its settings, and settings that are used for every command.
data Instructions
  = Instructions Dispatch Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- Settings that you need for every command
data Settings
  = Settings
      { settingPolite :: Bool
      }
  deriving (Show, Eq, Generic)

-- A sum type for the commands and their individual settings
data Dispatch
  = DispatchGreet GreetSettings
  deriving (Show, Eq, Generic)

-- A type per command for its settings.
-- You can omit this if the command does not need specific settings.
data GreetSettings
  = GreetSettings
      { greetSettingGreeting :: Maybe Text
      }
  deriving (Show, Eq, Generic)

-- Combine everything to instructions
combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  -- flag first, then environment, then config file
  -- default value if none of the above are used
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

-- What we find in the configuration variable.
-- Do nothing clever here, just represent the configuration file.
data Configuration
  = Configuration
      { configPolite :: Maybe Bool,
        configGreeting :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "polite" "Whether to be polite"
        <*> optionalField "greeting" "What to say when greeting"

-- Get the configuration based.
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

-- Where to get the configuration file by default.
-- This uses the XDG base directory specifictation: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|optparse-template|])
  resolveFile xdgConfigDir "config.yaml"

-- What we find in the configuration variable.
-- Do nothing clever here, just represent the relevant parts of the environment.
data Environment
  = Environment
      { envConfigFile :: Maybe FilePath,
        envPolite :: Maybe Bool,
        envGreeting :: Maybe Text
      }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.auto) "POLITE" (mE <> Env.help "Whether to be polite")
      <*> Env.var (fmap Just . Env.str) "GREETING" (mE <> Env.help "What to say when greeting")
  where
    mE = Env.def Nothing <> Env.keep

-- The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments Command Flags
  deriving (Show, Eq, Generic)

-- Get the command-line arguments
getArguments :: IO OptParse.Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  OptParse.handleParseResult result

-- The pure version of the argument parsing, to be used in testing
runArgumentsParser :: [String] -> OptParse.ParserResult Arguments
runArgumentsParser = OptParse.execParserPure prefs_ argParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

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
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- A sum type for the commands and their specific arguments
data Command
  = CommandGreet GreetArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "greet" $ CommandGreet <$> parseCommandGreet
      ]

-- The arguments specifically for the greet command
data GreetArgs
  = GreetArgs
      { greetArgGreeting :: Maybe Text
      }
  deriving (Show, Eq, Generic)

-- Don't worry about this code taking up a lot of space.
parseCommandGreet :: OptParse.ParserInfo GreetArgs
parseCommandGreet = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Greet the user"
    parser =
      ( GreetArgs
          <$> optional
            ( strOption
                ( mconcat
                    [ long "greeting",
                      help "What to say when greeting",
                      metavar "GREETING"
                    ]
                )
            )
      )

-- The flags for each command
data Flags
  = Flags
      { flagConfigFile :: Maybe FilePath,
        flagPolite :: Maybe Bool
      }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Give the path to an altenative config file",
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
