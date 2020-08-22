{-# LANGUAGE DeriveFunctor #-}

module Test.Smoke.Types.Results where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Files
import Test.Smoke.Types.Plans
import Test.Smoke.Types.Tests

type Results = [SuiteResult]

data SuiteResult
  = SuiteResultError SuiteName SuiteError
  | SuiteResult SuiteName (ResolvedPath Dir) [TestResult]

data TestResult
  = TestResult Test TestOutcome

data TestOutcome
  = TestSuccess
  | TestFailure
      TestPlan
      (PartResult Status)
      (PartResult StdOut)
      (PartResult StdErr)
      (Map (RelativePath File) (PartResult TestFileContents))
  | TestError SmokeError
  | TestIgnored

data PartResult a
  = PartSuccess
  | PartFailure (Vector (AssertFailure a))
  deriving (Functor)

isPartSuccess :: PartResult a -> Bool
isPartSuccess PartSuccess = True
isPartSuccess PartFailure {} = False
