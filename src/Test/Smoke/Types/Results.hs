module Test.Smoke.Types.Results where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Path
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Files
import Test.Smoke.Types.Plans
import Test.Smoke.Types.Tests

type Results = [SuiteResult]

data SuiteResult
  = SuiteResultError
      { suiteResultErrorSuiteName :: SuiteName
      , suiteResultError :: SmokeDiscoveryError
      }
  | SuiteResult
      { suiteResultSuiteName :: SuiteName
      , suiteResultLocation :: Path Abs Dir
      , suiteResultTestResults :: [TestResult]
      }

data TestResult =
  TestResult Test TestOutcome

data TestOutcome
  = TestSuccess
  | TestFailure
      TestPlan
      (PartResult Status)
      (PartResult StdOut)
      (PartResult StdErr)
      (Map (Path Rel File) (PartResult TestFileContents))
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
