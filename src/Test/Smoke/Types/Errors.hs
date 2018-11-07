module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Paths

data TestDiscoveryErrorMessage
  = NoSuchLocation Path
  | NoSuchTest Path
               TestName
  | CannotSelectTestInDirectory Path
                                TestName
  | InvalidSpecification Path
                         String
  deriving (Eq, Show)

instance Exception TestDiscoveryErrorMessage

data TestPlanErrorMessage
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentFixture Path
  | CouldNotReadFixture Path
                        String
  | NonExistentCommand Executable
  | PlanFilterError TestFilterErrorMessage
  deriving (Eq, Show)

instance Exception TestPlanErrorMessage

data TestFilterErrorMessage
  = NonExecutableFilter Executable
  | CouldNotExecuteFilter Executable
                          String
  | ExecutionFailed Executable
                    Status
                    StdOut
                    StdErr
  deriving (Eq, Show)

instance Exception TestFilterErrorMessage

data TestErrorMessage
  = NonExecutableCommand Executable
  | CouldNotExecuteCommand Executable
                           String
  | FilterError TestFilterErrorMessage
  | PlanError TestPlanErrorMessage
  | BlessError TestBlessErrorMessage
  | BlessIOException IOException
  deriving (Eq, Show)

instance Exception TestErrorMessage

data TestBlessErrorMessage
  = CouldNotBlessInlineFixture String
                               Text
  | CouldNotBlessFixtureWithFilter String
                                   Text
  | CouldNotBlessAMissingValue String
  | CouldNotBlessWithMultipleValues String
  deriving (Eq, Show)

instance Exception TestBlessErrorMessage
