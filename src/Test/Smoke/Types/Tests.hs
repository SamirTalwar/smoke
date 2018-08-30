{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Tests where

import Data.Aeson hiding (Options)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Fixtures

data Options = Options
  { optionsCommand :: Maybe Command
  , optionsTestLocations :: [FilePath]
  } deriving (Eq, Show)

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
      (v .:? "stdout" .!= Fixtures []) <*>
      (v .:? "stderr" .!= Fixtures []) <*>
      (InlineFixture . Status <$> v .:? "exit-status" .!= 0)

data Suite = Suite
  { suiteCommand :: Maybe Command
  , suiteTests :: [Test]
  } deriving (Eq, Show)

instance FromJSON Suite where
  parseJSON =
    withObject "Suite" $ \v -> Suite <$> (v .:? "command") <*> (v .: "tests")

type Suites = [(SuiteName, Suite)]

data Plan =
  Plan (Maybe Command)
       Suites

data TestExecutionPlan = TestExecutionPlan
  { planSuiteName :: SuiteName
  , planTest :: Test
  , planExecutable :: Executable
  , planArgs :: Args
  , planStdIn :: Maybe StdIn
  } deriving (Eq, Show)

type TestResults = [TestResult]

data TestResult
  = TestSuccess TestName
  | TestFailure TestName
                TestExecutionPlan
                (PartResult Status)
                (PartResult StdOut)
                (PartResult StdErr)
  | TestError TestName
              TestErrorMessage
  deriving (Eq, Show)

data PartResult a
  = PartSuccess
  | PartFailure [a]
                a
  deriving (Eq, Show)

instance Functor PartResult where
  _ `fmap` PartSuccess = PartSuccess
  f `fmap` (PartFailure expected actual) =
    PartFailure (map f expected) (f actual)
