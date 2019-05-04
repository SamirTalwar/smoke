module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import Path
import System.FilePath (FilePath)
import Test.Smoke.Types.Base

data SmokeError
  = DiscoveryError SmokeDiscoveryError
  | PlanningError SmokePlanningError
  | ExecutionError SmokeExecutionError
  | BlessError SmokeBlessError
  deriving (Eq, Show)

instance Exception SmokeError

data SmokeDiscoveryError
  = NoSuchLocation FilePath
  | NoSuchTest (Path Rel File)
               TestName
  | CannotSelectTestInDirectory (Path Rel Dir)
                                TestName
  | InvalidSpecification (Path Rel File)
                         String
  deriving (Eq, Show)

instance Exception SmokeDiscoveryError

data SmokePlanningError
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentFixture (Path Rel File)
  | CouldNotReadFixture (Path Rel File)
                        String
  | NonExistentCommand Executable
  | PlanningFilterError SmokeFilterError
  deriving (Eq, Show)

instance Exception SmokePlanningError

data SmokeExecutionError
  = NonExistentWorkingDirectory WorkingDirectory
  | NonExecutableCommand Executable
  | CouldNotExecuteCommand Executable
                           String
  | ExecutionFilterError SmokeFilterError
  deriving (Eq, Show)

instance Exception SmokeExecutionError

data SmokeBlessError
  = CouldNotBlessInlineFixture String
                               Text
  | CouldNotBlessAMissingValue String
  | CouldNotBlessWithMultipleValues String
  | BlessIOException IOException
  deriving (Eq, Show)

instance Exception SmokeBlessError

data SmokeFilterError
  = NonExecutableFilter Executable
  | CouldNotExecuteFilter Executable
                          String
  | ExecutionFailed Executable
                    Status
                    StdOut
                    StdErr
  deriving (Eq, Show)

instance Exception SmokeFilterError
