module Test.Smoke.Types where

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
  , testCommand :: Maybe Command
  , testArgs :: Maybe Args
  , testStdIn :: Maybe FilePath
  , testStdOut :: [FilePath]
  , testStdErr :: [FilePath]
  , testStatus :: Status
  } deriving (Eq, Show)

data TestExecutionPlan =
  TestExecutionPlan Test
                    String
                    Args
                    (Maybe String)
  deriving (Eq, Show)

type TestResults = [TestResult]

data TestResult
  = TestSuccess Test
  | TestFailure TestExecutionPlan
                ExpectedOutput
                ActualOutput
  | TestError Test
              TestErrorMessage
  deriving (Eq, Show)

data ExpectedOutput =
  ExpectedOutput Int
                 [String]
                 [String]
  deriving (Eq, Show)

data ActualOutput =
  ActualOutput Int
               String
               String
  deriving (Eq, Show)

data TestErrorMessage
  = NoCommandFile
  | NoInputFiles
  | NoOutputFiles
  | NonExistentCommand
  | NonExecutableCommand
  | CouldNotExecuteCommand String
  deriving (Eq, Show)
