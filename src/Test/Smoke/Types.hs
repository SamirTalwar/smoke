module Test.Smoke.Types
  ( Args
  , Command
  , Options(..)
  , Status
  , Test(..)
  , Tests
  , TestResult(..)
  , TestResults
  , TestErrorMessage(..)
  ) where

type Command = [String]

type Args = [String]

type Status = Int

data Options = Options
  { optionsCommand :: Maybe Command
  , optionsColor :: Bool
  , optionsTestLocations :: [FilePath]
  } deriving (Eq, Show)

type Tests = [Test]

data Test = Test
  { testName :: String
  , testCommand :: Command
  , testArgs :: Maybe Args
  , testStdIn :: Maybe FilePath
  , testStdOut :: [FilePath]
  , testStdErr :: [FilePath]
  , testStatus :: Status
  } deriving (Eq, Show)

type TestResults = [TestResult]

data TestResult
  = TestSuccess { testSuccessTest :: Test }
  | TestFailure { testFailureTest :: Test
                , testFailureActualStatus :: Status
                , testFailureActualStdOut :: String
                , testFailureActualStdErr :: String
                , testFailureStdIn :: Maybe String
                , testFailureExpectedStatus :: Status
                , testFailureExpectedStdOuts :: [String]
                , testFailureExpectedStdErrs :: [String] }
  | TestError { testErrorTest :: Test
              , testErrorMessage :: TestErrorMessage }
  deriving (Eq, Show)

data TestErrorMessage =
  CouldNotFindExecutable
  deriving (Eq, Show)
