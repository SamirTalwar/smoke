module Test.Smoke.Types.Summary where

data Summary
  = Summary
      { summarySuccesses :: Int,
        summaryFailures :: Int
      }
  deriving (Eq, Show)

summaryTotal :: Summary -> Int
summaryTotal (Summary successes failures) = successes + failures
