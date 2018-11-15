module Test.Smoke.Types.Results where

import Data.Vector (Vector)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Plans
import Test.Smoke.Types.Tests

type Results = [SuiteResult]

data SuiteResult = SuiteResult
  { suiteResultSuiteName :: SuiteName
  , suiteResultTestResults :: Either SmokeDiscoveryError [TestResult]
  }

data TestResult =
  TestResult Test
             TestOutcome

data TestOutcome
  = TestSuccess
  | TestFailure TestPlan
                (PartResult Status)
                (PartResult StdOut)
                (PartResult StdErr)
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
