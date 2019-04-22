{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Tests where

import Data.Aeson hiding (Options)
import Data.Vector (Vector)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Fixtures

data Options = Options
  { optionsCommand :: Maybe Command
  , optionsTestLocations :: Vector String
  } deriving (Eq, Show)

data TestSpecification =
  TestSpecification (Maybe Command)
                    Suites

type Suites = [(SuiteName, Either SmokeDiscoveryError Suite)]

data Suite = Suite
  { suiteCommand :: Maybe Command
  , suiteTests :: [Test]
  } deriving (Eq, Show)

instance FromJSON Suite where
  parseJSON =
    withObject "Suite" $ \v -> Suite <$> (v .:? "command") <*> (v .: "tests")

data Test = Test
  { testName :: TestName
  , testCommand :: Maybe Command
  , testArgs :: Maybe Args
  , testStdIn :: Maybe (Fixture StdIn)
  , testStdOut :: Fixtures StdOut
  , testStdErr :: Fixtures StdErr
  , testStatus :: Fixture Status
  } deriving (Eq, Show)

instance FromJSON Test where
  parseJSON =
    withObject "Test" $ \v ->
      Test <$> (TestName <$> v .: "name") <*> (v .:? "command") <*>
      (v .:? "args") <*>
      (v .:? "stdin") <*>
      (v .:? "stdout" .!= noFixtures) <*>
      (v .:? "stderr" .!= noFixtures) <*>
      (Fixture <$> (Inline . Status <$> v .:? "exit-status" .!= 0) <*>
       return Nothing)
