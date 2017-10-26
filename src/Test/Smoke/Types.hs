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
                (PartResult Status)
                (PartResult String)
                (PartResult String)
  | TestError Test
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

data TestErrorMessage
  = NoCommandFile
  | NoInputFiles
  | NoOutputFiles
  | NonExistentCommand
  | NonExecutableCommand
  | CouldNotExecuteCommand String
  deriving (Eq, Show)
