module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Executable

data SmokeError
  = DiscoveryError SmokeDiscoveryError
  | PlanningError SmokePlanningError
  | ExecutionError SmokeExecutionError
  | AssertionError SmokeAssertionError
  | BlessError SmokeBlessError
  deriving (Show)

instance Exception SmokeError

data SmokeDiscoveryError
  = NoSuchLocation FilePath
  | NoSuchTest (Path Relative File) TestName
  | CannotSelectTestInDirectory (Path Relative Dir) TestName
  | InvalidSpecification (Path Relative File) String
  deriving (Show)

instance Exception SmokeDiscoveryError

data SmokePlanningError
  = NoCommand
  | NoInput
  | NoOutput
  | PlanningFixtureFileError SmokeFileError
  | PlanningPathError PathError
  | PlanningFilterError SmokeFilterError
  deriving (Show)

instance Exception SmokePlanningError

data SmokeExecutionError
  = NonExistentWorkingDirectory WorkingDirectory
  | CouldNotExecuteCommand Executable IOError
  | CouldNotStoreDirectory (Path Resolved Dir) IOError
  | CouldNotRevertDirectory (Path Resolved Dir) IOError
  deriving (Show)

instance Exception SmokeExecutionError

newtype SmokeAssertionError
  = AssertionFilterError SmokeFilterError
  deriving (Show)

instance Exception SmokeAssertionError

data SmokeBlessError
  = CouldNotBlessInlineFixture FixtureName Text
  | CouldNotBlessAMissingValue FixtureName
  | CouldNotBlessWithMultipleValues FixtureName
  | CouldNotBlessContainsAssertion FixtureName Text
  | BlessIOException IOException
  deriving (Show)

instance Exception SmokeBlessError

data SmokeFilterError
  = MissingFilterScript
  | CouldNotExecuteFilter Executable IOError
  | FilterExecutionFailed Executable Status StdOut StdErr
  | FilterPathError PathError
  deriving (Show)

instance Exception SmokeFilterError

data SmokeFileError = MissingFile (Path Relative File) | CouldNotReadFile (Path Relative File) IOError
  deriving (Show)

instance Exception SmokeFileError

data SuiteError
  = SuiteDiscoveryError SmokeDiscoveryError
  | SuitePathError PathError
  deriving (Show)
