module Test.Smoke.Types.Errors where

import Control.Exception (Exception, IOException)
import Test.Smoke.Types.Base

data TestErrorMessage
  = NoCommand
  | NoInput
  | NoOutput
  | NonExistentCommand Executable
  | NonExecutableCommand Executable
  | CouldNotExecuteCommand Executable
                           String
  | CouldNotWriteFixture String
                         Contents
  | BlessingFailed IOException
  | CouldNotBlessAMissingValue String
  | CouldNotBlessWithMultipleValues String
  deriving (Eq, Show)

instance Exception TestErrorMessage
