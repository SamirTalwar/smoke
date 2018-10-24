module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Test.Smoke.Types.Base

data TestDiscoveryErrorMessage
  = NoSuchLocation FilePath
  | NoSuchTest FilePath
               TestName
  | CannotSelectTestInDirectory FilePath
                                TestName
  | InvalidSpecification FilePath
                         String
  deriving (Eq, Show)

instance Exception TestDiscoveryErrorMessage

data TestPlanErrorMessage
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentCommand Executable
  deriving (Eq, Show)

data TestErrorMessage
  = NonExecutableCommand Executable
  | CouldNotExecuteCommand Executable
                           String
  | PlanError TestPlanErrorMessage
  | BlessError TestBlessErrorMessage
  | BlessIOException IOException
  deriving (Eq, Show)

instance Exception TestErrorMessage

data TestBlessErrorMessage
  = CouldNotBlessInlineFixture String
                               Contents
  | CouldNotBlessAMissingValue String
  | CouldNotBlessWithMultipleValues String
  deriving (Eq, Show)

instance Exception TestBlessErrorMessage
