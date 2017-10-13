module Test.Smoke
  ( Args
  , Command
  , Options(..)
  , Test(..)
  , Tests
  , TestExecutionPlan(..)
  , TestResult(..)
  , TestResults
  , ExpectedOutput(..)
  , ActualOutput(..)
  , TestErrorMessage(..)
  , discoverTests
  , runTests
  ) where

import Test.Smoke.Discovery (discoverTests)
import Test.Smoke.Runner (runTests)
import Test.Smoke.Types
