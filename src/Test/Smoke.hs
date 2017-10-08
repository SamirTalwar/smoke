module Test.Smoke
  ( Command
  , Options(..)
  , Tests
  , Test(..)
  , discoverTests
  , runTests
  ) where

import Test.Smoke.Discovery (discoverTests)
import Test.Smoke.Types

runTests :: Tests -> IO ()
runTests tests = do
  print tests
  return ()
