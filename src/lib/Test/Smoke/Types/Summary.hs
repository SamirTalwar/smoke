module Test.Smoke.Types.Summary where

data Summary = Summary
  { summarySuccesses :: Int,
    summaryFailures :: Int,
    summaryIgnored :: Int
  }

summaryTotal :: Summary -> Int
summaryTotal (Summary successes failures ignored) = successes + failures + ignored
