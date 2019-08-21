module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import Path
import System.FilePath (FilePath)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Executable

data SmokeError
  = DiscoveryError SmokeDiscoveryError
  | PlanningError SmokePlanningError
  | ExecutionError SmokeExecutionError
  | BlessError SmokeBlessError
  deriving (Eq, Show)

instance Exception SmokeError

data SmokeDiscoveryError
  = NoSuchLocation FilePath
  | NoSuchTest (Path Rel File) TestName
  | CannotSelectTestInDirectory (Path Rel Dir) TestName
  | InvalidSpecification (Path Rel File) String
  deriving (Eq, Show)

instance Exception SmokeDiscoveryError

data SmokePlanningError
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentFixture (Path Rel File)
  | CouldNotReadFixture (Path Rel File) IOError
  | NonExistentCommand Executable
  | PlanningFilterError SmokeFilterError
  deriving (Eq, Show)

instance Exception SmokePlanningError

data SmokeExecutionError
  = NonExistentWorkingDirectory WorkingDirectory
  | NonExecutableCommand Executable
  | CouldNotExecuteCommand Executable IOError
  | CouldNotReadFile (Path Rel File) IOError
  | CouldNotStoreDirectory (Path Abs Dir) IOError
  | CouldNotRevertDirectory (Path Abs Dir) IOError
  | ExecutionFilterError SmokeFilterError
  deriving (Eq, Show)

instance Exception SmokeExecutionError

data SmokeBlessError
  = CouldNotBlessInlineFixture FixtureName Text
  | CouldNotBlessAMissingValue FixtureName
  | CouldNotBlessWithMultipleValues FixtureName
  | BlessIOException IOException
  deriving (Eq, Show)

instance Exception SmokeBlessError

data SmokeFilterError
  = MissingFilterScript
  | NonExecutableFilter Executable
  | CouldNotExecuteFilter Executable IOError
  | ExecutionFailed Executable Status StdOut StdErr
  deriving (Eq, Show)

instance Exception SmokeFilterError
