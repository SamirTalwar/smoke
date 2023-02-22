{-# LANGUAGE DeriveFunctor #-}

module Test.Smoke.Types.Results where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Plans
import Test.Smoke.Types.Tests

class IsSuccess a where
  isSuccess :: a -> Bool
  isFailure :: a -> Bool
  isFailure = not . isSuccess

type Results = [SuiteResult]

data SuiteResult
  = SuiteResultError SuiteName SuiteError
  | SuiteResult SuiteName (Path Resolved Dir) [TestResult]

data TestResult
  = TestFinished TestPlan FinishedTest
  | TestErrored Test SmokeError
  | TestIgnored Test

instance IsSuccess TestResult where
  isSuccess (TestFinished _ finishedTest) =
    isSuccess finishedTest
  isSuccess TestErrored {} =
    False
  isSuccess TestIgnored {} =
    False

data FinishedTest = FinishedTest
  { finishedTestStatus :: EqualityResult Status,
    finishedTestStdOut :: AssertionResult StdOut,
    finishedTestStdErr :: AssertionResult StdErr,
    finishedTestFiles :: Map (Path Relative File) (AssertionResult TestFileContents)
  }

instance IsSuccess FinishedTest where
  isSuccess (FinishedTest statusResult stdOutResult stdErrResult filesResults) =
    isSuccess statusResult && isSuccess stdOutResult && isSuccess stdErrResult && all isSuccess (Map.elems filesResults)

data EqualityResult a
  = EqualitySuccess
  | EqualityFailure (Expected a) (Actual a)
  deriving (Functor)

instance IsSuccess (EqualityResult a) where
  isSuccess EqualitySuccess = True
  isSuccess EqualityFailure {} = False

data AssertionResult a
  = AssertionSuccess
  | AssertionFailure (AssertionFailures a)
  deriving (Functor)

instance IsSuccess (AssertionResult a) where
  isSuccess AssertionSuccess = True
  isSuccess AssertionFailure {} = False

toAssertionResult :: EqualityResult a -> AssertionResult a
toAssertionResult EqualitySuccess = AssertionSuccess
toAssertionResult (EqualityFailure expected actual) = AssertionFailure $ SingleAssertionFailure $ AssertionFailureDiff expected actual
