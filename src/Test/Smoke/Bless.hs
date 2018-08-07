module Test.Smoke.Bless
  ( blessResults
  ) where

import Control.Exception (catch)
import Test.Smoke.Types

blessResults :: TestResults -> IO TestResults
blessResults = mapM blessResult

blessResult :: TestResult -> IO TestResult
blessResult (TestFailure TestExecutionPlan {planTest = test} _ (PartFailure (_:_:_) _) _) =
  return $ TestError test (CouldNotBlessWithMultipleValues "stdout")
blessResult (TestFailure TestExecutionPlan {planTest = test} _ _ (PartFailure (_:_:_) _)) =
  return $ TestError test (CouldNotBlessWithMultipleValues "stderr")
blessResult (TestFailure TestExecutionPlan {planTest = test} status stdOut stdErr) =
  do case status of
       PartFailure _ actual -> writeFixture (testStatus test) actual
       _ -> return ()
     case stdOut of
       PartFailure _ actual -> writeFixtures (testStdOut test) actual
       _ -> return ()
     case stdErr of
       PartFailure _ actual -> writeFixtures (testStdErr test) actual
       _ -> return ()
     return $ TestSuccess test
     `catch` \e -> return (TestError test (BlessingFailed e))
blessResult result = return result
