module Test.Smoke.Bless
  ( blessResults
  ) where

import qualified Data.ByteString as ByteString
import qualified Test.Smoke.FileTypes as FileTypes
import Test.Smoke.Types

blessResults :: TestResults -> IO TestResults
blessResults = mapM blessResult

blessResult :: TestResult -> IO TestResult
blessResult (TestFailure TestExecutionPlan {planTest = test} _ (PartFailure (_:_:_) _) _) =
  return $ TestError test CouldNotBlessStdOutWithMultipleValues
blessResult (TestFailure TestExecutionPlan {planTest = test} _ _ (PartFailure (_:_:_) _)) =
  return $ TestError test CouldNotBlessStdErrWithMultipleValues
blessResult (TestFailure (TestExecutionPlan test@(Test name location _ _ _ stdOutPaths stdErrPaths _) _ _ _) status stdOut stdErr) = do
  case status of
    PartFailure _ (Status actual) ->
      writeFile
        (FileTypes.filePath location name FileTypes.Status)
        (show actual ++ "\n")
    _ -> return ()
  case stdOut of
    PartFailure _ (StdOut actual) ->
      ByteString.writeFile (head stdOutPaths) actual
    _ -> return ()
  case stdErr of
    PartFailure _ (StdErr actual) ->
      ByteString.writeFile (head stdErrPaths) actual
    _ -> return ()
  return (TestSuccess test)
blessResult result = return result
