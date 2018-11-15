module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Paths

data SmokeError
  = DiscoveryError SmokeDiscoveryError
  | PlanningError SmokePlanningError
  | ExecutionError SmokeExecutionError
  | BlessError SmokeBlessError
  deriving (Eq, Show)

instance Exception SmokeError

data SmokeDiscoveryError
  = NoSuchLocation Path
  | NoSuchTest Path
               TestName
  | CannotSelectTestInDirectory Path
                                TestName
  | InvalidSpecification Path
                         String
  deriving (Eq, Show)

instance Exception SmokeDiscoveryError

data SmokePlanningError
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentFixture Path
  | CouldNotReadFixture Path
                        String
  | NonExistentCommand Executable
  | PlanningFilterError SmokeFilterError
  deriving (Eq, Show)

instance Exception SmokePlanningError

data SmokeExecutionError
  = NonExecutableCommand Executable
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
