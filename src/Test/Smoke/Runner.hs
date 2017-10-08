module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process (readProcessWithExitCode)
import Test.Smoke.Types

runTests :: Tests -> IO TestResults
runTests tests = forM tests runTest

runTest :: Test -> IO TestResult
runTest test = do
  stdIn <- sequence (readFile <$> testStdIn test)
  let expectedStatus = testStatus test
  expectedStdOuts <- ifEmpty "" <$> mapM readFile (testStdOut test)
  expectedStdErrs <- ifEmpty "" <$> mapM readFile (testStdErr test)
  let executableName = head <$> testCommand test
  executableExists <-
    fromMaybe False <$> sequence (doesFileExist <$> executableName)
  executable <-
    case (isJust executableName, executableExists) of
      (True, True) -> return executableName
      (True, False) -> findExecutable (fromJust executableName) -- TODO: Test this on Windows.
      (False, _) -> return Nothing
  eitherResult <-
    runExceptT $ do
      when (isNothing (testCommand test)) $ throwE NoCommandFile
      when (isNothing (testArgs test) && isNothing (testStdIn test)) $
        throwE NoInputFiles
      when (null (testStdOut test) && null (testStdErr test)) $
        throwE NoOutputFiles
      when (isNothing executable) $ throwE NonExistentCommand
      let args =
            tail (fromJust (testCommand test)) ++ fromMaybe [] (testArgs test)
      (actualExitCode, actualStdOut, actualStdErr) <-
        do execution <-
             liftIO
               (tryIOError
                  (readProcessWithExitCode
                     (fromJust executable)
                     args
                     (fromMaybe "" stdIn)))
           case execution of
             Left e ->
               if isPermissionError e
                 then throwE NonExecutableCommand
                 else throwE $ CouldNotExecuteCommand (show e)
             Right outputs -> return outputs
      let actualStatus = convertExitCode actualExitCode
      if actualStatus == expectedStatus &&
         actualStdOut `elem` expectedStdOuts &&
         actualStdErr `elem` expectedStdErrs
        then return $ TestSuccess test
        else return
               TestFailure
               { testFailureTest = test
               , testFailureActualStatus = actualStatus
               , testFailureActualStdOut = actualStdOut
               , testFailureActualStdErr = actualStdErr
               , testFailureStdIn = stdIn
               , testFailureExpectedStatus = expectedStatus
               , testFailureExpectedStdOuts = expectedStdOuts
               , testFailureExpectedStdErrs = expectedStdErrs
               }
  case eitherResult of
    Left message -> return $ TestError test message
    Right result -> return result

ifEmpty :: a -> [a] -> [a]
ifEmpty value [] = [value]
ifEmpty _ xs = xs

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = 0
convertExitCode (ExitFailure value) = value
