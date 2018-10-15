module Test.Smoke.Types.Results where

import Data.Vector (Vector)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Tests

type Results = [SuiteResult]

data SuiteResult = SuiteResult
  { suiteResultSuiteName :: SuiteName
  , suiteResultTestResults :: [TestResult]
  }

data TestResult
  = TestSuccess TestName
  | TestFailure TestName
                TestExecutionPlan
                (PartResult Status)
                (PartResult StdOut)
                (PartResult StdErr)
  | TestError TestName
              TestErrorMessage
  deriving (Eq, Show)

data PartResult a
  = PartSuccess
  | PartFailure (Vector a)
                a
  deriving (Eq, Show)

instance Functor PartResult where
  _ `fmap` PartSuccess = PartSuccess
  f `fmap` (PartFailure expected actual) =
    PartFailure (f <$> expected) (f actual)
