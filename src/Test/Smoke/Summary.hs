module Test.Smoke.Summary
  ( summarizeResults
  ) where

import Test.Smoke.Types

summarizeResults :: Results -> Summary
summarizeResults results = Summary successes failures
  where
    allTestResults = concatMap suiteResultTestResults results
    total = length allTestResults
    failures = length $ filter isFailure allTestResults
    successes = total - failures

isFailure :: TestResult -> Bool
isFailure (TestResult _ TestSuccess) = False
isFailure (TestResult _ TestFailure {}) = True
isFailure (TestResult _ TestError {}) = True
