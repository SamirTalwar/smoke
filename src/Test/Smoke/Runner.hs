module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process (readProcessWithExitCode)
import Test.Smoke.Types

type Execution = ExceptT TestErrorMessage IO

type ExecutionOutputs = (ExitCode, String, String)

runTests :: Tests -> IO TestResults
runTests tests = forM tests runTest

runTest :: Test -> IO TestResult
runTest test =
  handleError (TestError test) <$>
  runExceptT
    (do validateTest test
        executionPlan <- readExecutionPlan test
        expectedOutput <- liftIO $ readExpectedOutputs test
        actualOutput <- executeTest executionPlan
        return $ processOutput executionPlan expectedOutput actualOutput)

validateTest :: Test -> Execution ()
validateTest test = do
  when (isNothing (testCommand test)) $ throwE NoCommandFile
  when (isNothing (testArgs test) && isNothing (testStdIn test)) $
    throwE NoInputFiles
  when (null (testStdOut test) && null (testStdErr test)) $ throwE NoOutputFiles
  return ()

readExecutionPlan :: Test -> Execution TestExecutionPlan
readExecutionPlan test = do
  executableName <- onNothingThrow NoCommandFile (head <$> testCommand test)
  executableExists <- liftIO (doesFileExist executableName)
  executable <-
    if executableExists
      then return executableName
      else onNothingThrow NonExistentCommand =<<
           liftIO (findExecutable executableName)
  let args = tail (fromJust (testCommand test)) ++ fromMaybe [] (testArgs test)
  stdIn <- liftIO $ sequence (readFile <$> testStdIn test)
  return $ TestExecutionPlan test executable args stdIn

readExpectedOutputs :: Test -> IO ExpectedOutput
readExpectedOutputs test = do
  let expectedStatus = testStatus test
  expectedStdOuts <- ifEmpty "" <$> mapM readFile (testStdOut test)
  expectedStdErrs <- ifEmpty "" <$> mapM readFile (testStdErr test)
  return $ ExpectedOutput expectedStatus expectedStdOuts expectedStdErrs

executeTest :: TestExecutionPlan -> Execution ExecutionOutputs
executeTest (TestExecutionPlan _ executable args stdIn) =
  liftIO
    (tryIOError (readProcessWithExitCode executable args (fromMaybe "" stdIn))) >>=
  handleExecutionError

handleExecutionError :: Either IOError a -> Execution a
handleExecutionError (Left e) =
  if isPermissionError e
    then throwE NonExecutableCommand
    else throwE $ CouldNotExecuteCommand (show e)
handleExecutionError (Right value) = return value

processOutput ::
     TestExecutionPlan
  -> ExpectedOutput
  -> (ExitCode, String, String)
  -> TestResult
processOutput executionPlan@(TestExecutionPlan test _ _ _) expectedOutput@(ExpectedOutput expectedStatus expectedStdOuts expectedStdErrs) (actualExitCode, actualStdOut, actualStdErr) =
  if actualStatus == expectedStatus &&
     actualStdOut `elem` expectedStdOuts && actualStdErr `elem` expectedStdErrs
    then TestSuccess test
    else TestFailure executionPlan expectedOutput actualOutput
  where
    actualStatus = convertExitCode actualExitCode
    actualOutput = ActualOutput actualStatus actualStdOut actualStdErr

handleError :: (a -> b) -> Either a b -> b
handleError handler = either handler id

onNothingThrow :: Monad m => e -> Maybe a -> ExceptT e m a
onNothingThrow _ (Just value) = return value
onNothingThrow exception Nothing = throwE exception

ifEmpty :: a -> [a] -> [a]
ifEmpty value [] = [value]
ifEmpty _ xs = xs

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = 0
convertExitCode (ExitFailure value) = value
