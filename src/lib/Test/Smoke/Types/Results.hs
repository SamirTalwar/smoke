{-# LANGUAGE DeriveFunctor #-}

module Test.Smoke.Types.Results where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  | SuiteResult SuiteName (ResolvedPath Dir) [TestResult]

data TestResult
  = TestResult
      { resultPlan :: TestPlan,
        resultStatus :: EqualityResult Status,
        resultStdOut :: AssertionResult StdOut,
        resultStdErr :: AssertionResult StdErr,
        resultFiles :: Map (RelativePath File) (AssertionResult TestFileContents)
      }
  | TestError Test SmokeError
  | TestIgnored Test

instance IsSuccess TestResult where
  isSuccess (TestResult _ statusResult stdOutResult stdErrResult filesResults) =
    isSuccess statusResult && isSuccess stdOutResult && isSuccess stdErrResult && all isSuccess (Map.elems filesResults)
  isSuccess TestError {} =
    False
  isSuccess TestIgnored {} =
    False

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
