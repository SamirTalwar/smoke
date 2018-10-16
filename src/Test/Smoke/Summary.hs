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
    allResults =
      concatMap
        (either (const [Failure]) (map summarizeResult) . suiteResultTestResults)
        results
    total = length allResults
    failures = length $ filter (== Failure) allResults
    successes = total - failures

summarizeResult :: TestResult -> SummaryResult
summarizeResult (TestResult _ TestSuccess) = Success
summarizeResult (TestResult _ TestFailure {}) = Failure
summarizeResult (TestResult _ TestError {}) = Failure
