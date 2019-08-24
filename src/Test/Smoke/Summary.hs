module Test.Smoke.Summary
  ( summarizeResults
  ) where

import Test.Smoke.Types

data SummaryResult
  = Success
  | Failure
  deriving (Eq, Show)

summarizeResults :: Results -> Summary
summarizeResults results = Summary successes failures
  where
    summarizeSuiteResult (SuiteResultDiscoveryError _ _) = [Failure]
    summarizeSuiteResult (SuiteResultExecutableError _ _) = [Failure]
    summarizeSuiteResult (SuiteResult _ _ testResults) =
      map summarizeResult testResults
    allResults = concatMap summarizeSuiteResult results
    total = length allResults
    failures = length $ filter (== Failure) allResults
    successes = total - failures

summarizeResult :: TestResult -> SummaryResult
summarizeResult (TestResult _ TestSuccess) = Success
summarizeResult (TestResult _ TestFailure {}) = Failure
summarizeResult (TestResult _ TestError {}) = Failure
