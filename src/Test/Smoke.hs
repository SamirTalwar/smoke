module Test.Smoke
  ( Args
  , Command
  , Options(..)
  , Tests
  , Test(..)
  , discoverTests
  , runTests
  ) where

import Test.Smoke.Discovery (discoverTests)
import Test.Smoke.Runner (runTests)
import Test.Smoke.Types
