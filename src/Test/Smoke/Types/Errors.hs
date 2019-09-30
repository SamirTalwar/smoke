module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import System.FilePath (FilePath)
import Test.Smoke.Paths
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
  | NoSuchTest (RelativePath File) TestName
  | CannotSelectTestInDirectory (RelativePath Dir) TestName
  | InvalidSpecification (RelativePath File) String
  deriving (Eq, Show)

instance Exception SmokeDiscoveryError

data SmokePlanningError
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentFixture (RelativePath File)
  | CouldNotReadFixture (RelativePath File) IOError
  | PlanningPathError PathError
  | PlanningFilterError SmokeFilterError
  deriving (Eq, Show)

instance Exception SmokePlanningError

data SmokeExecutionError
  = NonExistentWorkingDirectory WorkingDirectory
  | CouldNotExecuteCommand Executable IOError
  | CouldNotReadFile (RelativePath File) IOError
  | CouldNotStoreDirectory (ResolvedPath Dir) IOError
  | CouldNotRevertDirectory (ResolvedPath Dir) IOError
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
  | CouldNotExecuteFilter Executable IOError
  | ExecutionFailed Executable Status StdOut StdErr
  | FilterPathError PathError
  deriving (Eq, Show)

instance Exception SmokeFilterError

data SuiteError
  = SuiteDiscoveryError SmokeDiscoveryError
  | SuitePathError PathError
  deriving (Eq, Show)
