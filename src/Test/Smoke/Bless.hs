module Test.Smoke.Bless
  ( blessResults
  ) where

import Control.Exception (catch)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist, removeFile)
import Test.Smoke.FileTypes (filePath)
import qualified Test.Smoke.FileTypes as FileTypes
import Test.Smoke.Types

blessResults :: TestResults -> IO TestResults
blessResults = mapM blessResult

blessResult :: TestResult -> IO TestResult
blessResult (TestFailure TestExecutionPlan {planTest = test} _ (PartFailure (_:_:_) _) _) =
  return $ TestError test CouldNotBlessStdOutWithMultipleValues
blessResult (TestFailure TestExecutionPlan {planTest = test} _ _ (PartFailure (_:_:_) _)) =
  return $ TestError test CouldNotBlessStdErrWithMultipleValues
blessResult (TestFailure (TestExecutionPlan test@(Test name location _ _ _ stdOutPaths stdErrPaths _) _ _ _) status stdOut stdErr) =
  do case status of
       PartFailure _ (Status 0) -> removeFile statusPath
       PartFailure _ (Status actual) ->
         writeFile statusPath (show actual ++ "\n")
       _ -> return ()
     case stdOut of
       PartFailure _ (StdOut actual) -> writeOutput stdOutPath actual
       _ -> return ()
     case stdErr of
       PartFailure _ (StdErr actual) -> writeOutput stdErrPath actual
       _ -> return ()
     stdOutExists <- doesFileExist stdOutPath
     stdErrExists <- doesFileExist stdErrPath
     unless (stdOutExists || stdErrExists) $ writeOutput stdOutPath Text.empty
     return $ TestSuccess test
     `catch` \e -> return (TestError test (BlessingFailed e))
  where
    statusPath = filePath location name FileTypes.Status
    stdOutPath =
      fromMaybe
        (filePath location name FileTypes.StdOut)
        (listToMaybe stdOutPaths)
    stdErrPath =
      fromMaybe
        (filePath location name FileTypes.StdErr)
        (listToMaybe stdErrPaths)
blessResult result = return result

writeOutput :: FilePath -> Contents -> IO ()
writeOutput path contents
  | contents == Text.empty = removeFile path
  | otherwise = TextIO.writeFile path contents
