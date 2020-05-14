module Test.Smoke.Summary
  ( summarizeResults,
  )
where

import Test.Smoke.Types

data SummaryResult
  = Success
  | Failure
  | Ignored
  deriving (Eq, Show)

summarizeResults :: Results -> Summary
summarizeResults results = Summary successes failures ignored
  where
    summarizeSuiteResult (SuiteResultError _ _) = [Failure]
    summarizeSuiteResult (SuiteResult _ _ testResults) =
      map summarizeResult testResults
    allResults = concatMap summarizeSuiteResult results
    successes = length $ filter (== Success) allResults
    failures = length $ filter (== Failure) allResults
    ignored = length $ filter (== Ignored) allResults

summarizeResult :: TestResult -> SummaryResult
summarizeResult (TestResult _ TestSuccess) = Success
summarizeResult (TestResult _ TestFailure {}) = Failure
summarizeResult (TestResult _ TestError {}) = Failure
summarizeResult (TestResult _ TestIgnored) = Ignored
