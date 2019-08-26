module Test.Smoke.Types.Results where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Files
import Test.Smoke.Types.Plans
import Test.Smoke.Types.Tests

type Results = [SuiteResult]

data SuiteResult
  = SuiteResultError SuiteName SuiteError
  | SuiteResult SuiteName (ResolvedPath Dir) [TestResult]
  deriving (Eq, Show)

data TestResult =
  TestResult Test TestOutcome
  deriving (Eq, Show)

data TestOutcome
  = TestSuccess
  | TestFailure
      TestPlan
      (PartResult Status)
      (PartResult StdOut)
      (PartResult StdErr)
      (Map (RelativePath File) (PartResult TestFileContents))
  | TestError SmokeError
  deriving (Eq, Show)

data PartResult a
  = PartSuccess
  | PartFailure (Vector (a, a))
  deriving (Eq, Show)

instance Functor PartResult where
  _ `fmap` PartSuccess = PartSuccess
  f `fmap` (PartFailure failures) =
    PartFailure ((\(expected, actual) -> (f expected, f actual)) <$> failures)
