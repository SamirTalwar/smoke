module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.Types

type Execution = ExceptT TestErrorMessage IO

type ExpectedOutputs = (Status, [StdOut], [StdErr])

type ActualOutputs = (Status, StdOut, StdErr)

runTests :: Plan -> IO TestResults
runTests (Plan planCommand specs) = do
  results <-
    forM specs $ \(Suites suites) ->
      forM suites $ \(suiteName, Suite suiteCommand tests) ->
        forM tests (runTest suiteName (suiteCommand <|> planCommand))
  return $ concat $ concat results

runTest :: Maybe SuiteName -> Maybe Command -> Test -> IO TestResult
runTest suiteName defaultCommand test =
  handleError (TestError (testName test)) <$>
  runExceptT
    (do validateTest defaultCommand test
        executionPlan <- readExecutionPlan suiteName defaultCommand test
        expectedOutput <- liftIO $ readExpectedOutputs test
        actualOutput <- executeTest executionPlan
        return $ processOutput executionPlan expectedOutput actualOutput)

validateTest :: Maybe Command -> Test -> Execution ()
validateTest defaultCommand test = do
  when (isNothing (testCommand test <|> defaultCommand)) $ throwE NoCommand
  when (isNothing (testArgs test) && isNothing (testStdIn test)) $
    throwE NoInput
  when (isEmpty (testStdOut test) && isEmpty (testStdErr test)) $
    throwE NoOutput
  where
    isEmpty (Fixtures []) = True
    isEmpty Fixtures {} = False

readExecutionPlan ::
     Maybe SuiteName -> Maybe Command -> Test -> Execution TestExecutionPlan
readExecutionPlan suiteName defaultCommand test = do
  command <- onNothingThrow NoCommand (testCommand test <|> defaultCommand)
  let executableName = head command
  executableExists <- liftIO (doesFileExist executableName)
  executable <-
    if executableExists
      then return executableName
      else onNothingThrow (NonExistentCommand executableName) =<<
           liftIO (findExecutable executableName)
  let args = tail command ++ fromMaybe [] (testArgs test)
  stdIn <- liftIO $ sequence $ readFixture <$> testStdIn test
  return $ TestExecutionPlan suiteName test executable args stdIn

readExpectedOutputs :: Test -> IO ExpectedOutputs
readExpectedOutputs test = do
  expectedStatus <- readFixture (testStatus test)
  expectedStdOuts <-
    ifEmpty (StdOut Text.empty) <$> readFixtures (testStdOut test)
  expectedStdErrs <-
    ifEmpty (StdErr Text.empty) <$> readFixtures (testStdErr test)
  return (expectedStatus, expectedStdOuts, expectedStdErrs)

executeTest :: TestExecutionPlan -> Execution ActualOutputs
executeTest (TestExecutionPlan _ _ executable args stdIn) = do
  (exitCode, processStdOut, processStdErr) <-
    handleExecutionError executable =<<
    liftIO (tryIOError (readProcessWithExitCode executable args processStdIn))
  return (convertExitCode exitCode, StdOut processStdOut, StdErr processStdErr)
  where
    processStdIn = unStdIn $ fromMaybe (StdIn Text.empty) stdIn

handleExecutionError :: Executable -> Either IOError a -> Execution a
handleExecutionError executable (Left e) =
  if isPermissionError e
    then throwE $ NonExecutableCommand executable
    else throwE $ CouldNotExecuteCommand executable (show e)
handleExecutionError _ (Right value) = return value

processOutput ::
     TestExecutionPlan -> ExpectedOutputs -> ActualOutputs -> TestResult
processOutput executionPlan@(TestExecutionPlan suiteName test _ _ _) (expectedStatus, expectedStdOuts, expectedStdErrs) (actualStatus, actualStdOut, actualStdErr) =
  if statusResult == PartSuccess &&
     stdOutResult == PartSuccess && stdErrResult == PartSuccess
    then TestSuccess name
    else TestFailure name executionPlan statusResult stdOutResult stdErrResult
  where
    name = maybe (testName test) (<> "/" <> testName test) suiteName
    statusResult =
      if expectedStatus == actualStatus
        then PartSuccess
        else PartFailure [expectedStatus] actualStatus
    stdOutResult =
      if actualStdOut `elem` expectedStdOuts
        then PartSuccess
        else PartFailure expectedStdOuts actualStdOut
    stdErrResult =
      if actualStdErr `elem` expectedStdErrs
        then PartSuccess
        else PartFailure expectedStdErrs actualStdErr

readFixture :: FixtureContents a => Fixture a -> IO a
readFixture (InlineFixture contents) = return contents
readFixture (FileFixture path) = deserializeFixture <$> TextIO.readFile path

readFixtures :: FixtureContents a => Fixtures a -> IO [a]
readFixtures (Fixtures fixtures) = mapM readFixture fixtures

handleError :: (a -> b) -> Either a b -> b
handleError handler = either handler id

onNothingThrow :: Monad m => e -> Maybe a -> ExceptT e m a
onNothingThrow _ (Just value) = return value
onNothingThrow exception Nothing = throwE exception

ifEmpty :: a -> [a] -> [a]
ifEmpty value [] = [value]
ifEmpty _ xs = xs

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value
