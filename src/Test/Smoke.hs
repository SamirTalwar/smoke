module Test.Smoke (
  runSmokeTests
) where

type Executable = String
type TestCases = FilePath

runSmokeTests :: Executable -> TestCases -> IO ()
runSmokeTests _ _ = return ()
