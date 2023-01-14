module Test.Smoke.Summary
  ( summarizeResults,
    summaryTotal,
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
summarizeResult result@TestFinished {} =
  if isSuccess result then Success else Failure
summarizeResult TestErrored {} = Failure
summarizeResult TestIgnored {} = Ignored

summaryTotal :: Summary -> Int
summaryTotal (Summary successes failures ignored) = successes + failures + ignored
