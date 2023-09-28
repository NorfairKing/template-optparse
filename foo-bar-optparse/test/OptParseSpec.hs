{-# LANGUAGE OverloadedStrings #-}

-- | = Optparse Tests Template
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
-- This module contains tests for the 'OptParse' module.
-- It is usually not necessary to write tests for your option parsing, but when
-- a bug occurs, you definitely want to be able to write regression tests, so
-- in this module we show one test for each of the arguments, environment
-- variables and configuration file.
module OptParseSpec
  ( spec,
  )
where

import Data.Yaml as Yaml
import qualified Env
import OptParse
import Options.Applicative
import Test.Hspec

spec :: Spec
spec = do
  describe "Arguments" $
    it "parses 'greet --greeting hello --polite' correctly" $
      do
        let args = ["greet", "--greeting", "hello", "--polite"]
        case execParserPure prefs_ argParser args of
          CompletionInvoked _ -> expectationFailure "Completion invoked"
          Failure err -> expectationFailure $ unlines ["Failed to parse arguments: ", show err]
          Success a ->
            a
              `shouldBe` Arguments
                ( CommandGreet
                    GreetArgs
                      { greetArgGreeting = Just "hello"
                      }
                )
                ( Flags
                    { flagConfigFile = Nothing,
                      flagPolite = Just True
                    }
                )
  describe "Environment" $
    it "parses FOO_BAR_GREETING and FOO_BAR_POLITE correctly" $
      do
        let env = [("FOO_BAR_GREETING", "hello"), ("FOO_BAR_POLITE", "True")]
        case Env.parsePure environmentParser env of
          Left err -> expectationFailure $ unlines ["Failed to parse environment variables: ", show err]
          Right e ->
            e
              `shouldBe` ( Environment
                             { envConfigFile = Nothing,
                               envGreeting = Just "hello",
                               envPolite = Just True
                             }
                         )
  describe "Configuration" $
    it "parses 'greeting' and 'polite' correctly" $
      do
        let config = object [("greeting", "hello"), ("polite", toJSON True)]
        case parseEither parseJSON config of
          Left err -> expectationFailure $ unlines ["Failed to parse configuration: ", show err]
          Right c ->
            c
              `shouldBe` Configuration
                { configPolite = Just True,
                  configGreeting = Just "hello"
                }
