module Test.Smoke.Runner
  ( runTests
  ) where

import Test.Smoke.Types (Tests)

runTests :: Tests -> IO ()
runTests tests = do
  print tests
  return ()
