module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Monad (forM)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
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
  executable <- findExecutable (head (testCommand test)) -- TODO: Test this on Windows.
  if isNothing executable
    then return $ TestError test CouldNotFindExecutable
    else do
      let args = tail (testCommand test) ++ fromMaybe [] (testArgs test)
      (actualExitCode, actualStdOut, actualStdErr) <-
        readProcessWithExitCode (fromJust executable) args (fromMaybe "" stdIn)
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

ifEmpty :: a -> [a] -> [a]
ifEmpty value [] = [value]
ifEmpty _ xs = xs

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = 0
convertExitCode (ExitFailure value) = value
