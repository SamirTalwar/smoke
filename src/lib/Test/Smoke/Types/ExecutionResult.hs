module Test.Smoke.Types.ExecutionResult where

import Data.Map.Strict (Map)
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors

data ExecutionResult
  = ExecutionSucceeded ActualOutputs
  | ExecutionIgnored
  | ExecutionFailed SmokeExecutionError

data ActualOutputs = ActualOutputs Status StdOut StdErr ActualFiles

type ActualFiles = Map (Path Resolved File) ActualFile

data ActualFile = ActualFileContents TestFileContents | ActualFileError SmokeFileError
