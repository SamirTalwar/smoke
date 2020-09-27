{-# LANGUAGE DeriveFunctor #-}

module Test.Smoke.Types.Results where

import Data.Map.Strict (Map)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Plans
import Test.Smoke.Types.Tests

type Results = [SuiteResult]

data SuiteResult
  = SuiteResultError SuiteName SuiteError
  | SuiteResult SuiteName (ResolvedPath Dir) [TestResult]

data TestResult
  = TestSuccess Test
  | TestFailure
      TestPlan
      (EqualityResult Status)
      (AssertionResult StdOut)
      (AssertionResult StdErr)
      (Map (RelativePath File) (AssertionResult TestFileContents))
  | TestError Test SmokeError
  | TestIgnored Test

class IsSuccess a where
  isSuccess :: a -> Bool
  isFailure :: a -> Bool
  isFailure = not . isSuccess

data EqualityResult a
  = EqualitySuccess
  | EqualityFailure {equalityFailureExpected :: a, equalityFailureActual :: a}
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
