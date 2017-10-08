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
  stdIn <- fromMaybe (return "") (readFile <$> testStdIn test)
  expectedStdOuts <- ifEmpty "" <$> mapM readFile (testStdOut test)
  expectedStdErrs <- ifEmpty "" <$> mapM readFile (testStdErr test)
  executable <- findExecutable (head (testCommand test)) -- TODO: Test this on Windows.
  if isNothing executable
    then return $ TestError test CouldNotFindExecutable
    else do
      let args = tail (testCommand test) ++ fromMaybe [] (testArgs test)
      (actualExitCode, actualStdOut, actualStdErr) <-
        readProcessWithExitCode (fromJust executable) args stdIn
      let actualStatus = convertExitCode actualExitCode
      if testStatus test == actualStatus &&
         actualStdOut `elem` expectedStdOuts &&
         actualStdErr `elem` expectedStdErrs
        then return $ TestSuccess test
        else return $ TestFailure test actualStatus actualStdOut actualStdErr

ifEmpty :: a -> [a] -> [a]
ifEmpty value [] = [value]
ifEmpty _ xs = xs

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = 0
convertExitCode (ExitFailure value) = value
