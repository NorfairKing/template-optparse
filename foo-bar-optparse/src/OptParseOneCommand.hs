{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | = Optparse Template for a single command
--
-- This is a template implementation of flags, options, environment variable and configuration file parsing according to best practices.
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
-- * Flags, Options: Whatever you put after the command on the command-line.
-- * Environment: Environment variables
-- * Configuration: Whatever you find in the configuration file
--
-- For the purposes of this code, we use the following types:
--
-- * 'Flags': A product type for the flags and options that you specify on the command-line.
-- * 'Environment': A product type for the relevant environment variables.
-- * 'Configuration': A product type for the relevant parts of the configuration file.
-- * 'Settings': A product type for the settings. This is what your program will use.
--
-- === High-level workings
--
-- The gathering of the 'Settings' happens in two stages.
--
-- In the first stage, the 'Flags', 'Environment' and 'Configuration' values are gathered from their respective places.
-- At this point, these types should represent what is found, without any processing or validation.
--
-- In the second stage, in the 'combineToSettings' function, all these variables are combined together to produce the 'Settings'.
-- It is in this phase that we do processing, validation and even 'System.Exit.die' if necessary.
--
-- === Included Example
--
-- This template comes with an example implementation for the 'OptParse' module for web server.
-- It accepts a port setting on in all three ways.
--
-- === 'FilePath' Example
--
-- As an example, suppose our program uses a cache file as part of the server
--
-- 1. Add a field to the 'Flags' type to indicate that the user may specify this file on the command-line.
-- 2. Add a field to the 'Environment' type to indicate that the user may specify this file in an environment variable.
-- 3. Add a field to the 'Configuration' type to indicate that the user may specify this file in the configuration file as well.
-- 4. Add a field to the 'Settings' type that your program will use.
-- 5. Combine the all three in the 'combineToSettings' function.
--
--
-- === Further instructions and support
--
-- Continue reading in the code inline for more detailed instructions.
--
-- If you have any trouble, you can contact @syd@ at @cs-syd@ dot @eu@ for support.
module OptParseOneCommand
  ( -- * Interface
    getSettings,
    Settings (..),

    -- ** Exposed for testing
    combineToSettings,
    getFlags,
    prefs_,
    flagsParser,
    parseFlags,
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
import qualified Data.Text as T
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

-- | A product type for the settings that your program will use
data Settings = Settings
  { settingPort :: !Int
  }
  deriving (Show, Eq, Generic)

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  -- This is a typical way to combine a setting.
  --
  -- We choose the first of the supplied flag, environment variable or configuration field,
  -- or default value if none of the those were supplied.
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc configPort
  pure Settings {..}
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
  { configPort :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

-- | We use @autodocodec@ for parsing a YAML config.
instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "port" "The port to serve requests on"
          .= configPort

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
    envPort :: !(Maybe Int)
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
      <*> optional (Env.var Env.auto "PORT" (Env.help "The port to serve requests on"))

-- | Get the command-line flags
getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Flags'
flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
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

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int)
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
      ( option
          auto
          ( mconcat
              [ long "port",
                help "The port to serve requests on"
              ]
          )
      )
