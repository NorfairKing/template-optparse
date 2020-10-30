{-# LANGUAGE OverloadedStrings #-}

-- | = Optparse Tests Template
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
-- Copyright (c) 2020 Tom Sydney Kerckhove.
--
-- All Rights Reserved.
--
-- == Instructions
--
-- This module contains tests for the 'OptParseOneCommand' module.
-- It is usually not necessary to write tests for your option parsing, but when
-- a bug occurs, you definitely want to be able to write regression tests, so
-- in this module we show one test for each of the arguments, environment
-- variables and configuration file.
module OptParseOneCommandSpec
  ( spec,
  )
where

import Data.Yaml as Yaml
import qualified Env
import OptParseOneCommand
import Options.Applicative
import Test.Hspec

spec :: Spec
spec = do
  describe "Arguments"
    $ it "parses '--port 3000' correctly"
    $ do
      let args = ["--port", "3000"]
      case execParserPure prefs_ flagsParser args of
        CompletionInvoked _ -> expectationFailure "Completion invoked"
        Failure err -> expectationFailure $ unlines ["Failed to parse arguments: ", show err]
        Success a ->
          a
            `shouldBe` ( Flags
                           { flagConfigFile = Nothing,
                             flagPort = Just 3000
                           }
                       )
  describe "Environment"
    $ it "parses FOOBAR_PORT correctly"
    $ do
      let env = [("FOOBAR_PORT", "3000")]
      case Env.parsePure environmentParser env of
        Left err -> expectationFailure $ unlines ["Failed to parse environment variables: ", show err]
        Right e ->
          e
            `shouldBe` ( Environment
                           { envConfigFile = Nothing,
                             envPort = Just 3000
                           }
                       )
  describe "Configuration"
    $ it "parses 'port'  correctly"
    $ do
      let config = object [("port", toJSON (3000 :: Int))]
      case parseEither parseJSON config of
        Left err -> expectationFailure $ unlines ["Failed to parse configuration: ", show err]
        Right c ->
          c
            `shouldBe` ( Configuration
                           { configPort = Just 3000
                           }
                       )