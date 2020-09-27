module Test.Smoke.Types.Summary where

data Summary = Summary
  { summarySuccesses :: Int,
    summaryFailures :: Int,
    summaryIgnored :: Int
  }
