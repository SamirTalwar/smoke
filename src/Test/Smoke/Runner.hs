module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.Errors
import Test.Smoke.Types

type Execution = ExceptT TestErrorMessage IO

type ExpectedOutputs = (Status, Vector StdOut, Vector StdErr)

type ActualOutputs = (Status, StdOut, StdErr)

runTests :: Plan -> IO Results
runTests (Plan planCommand suites) =
  forM suites $ \(suiteName, Suite command tests) -> do
    testResults <- forM tests (runTest (command <|> planCommand))
    return $ SuiteResult suiteName testResults

runTest :: Maybe Command -> Test -> IO TestResult
runTest defaultCommand test =
  handleError (TestError (testName test)) <$>
  runExceptT
    (do validateTest defaultCommand test
        executionPlan <- readExecutionPlan defaultCommand test
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
    isEmpty (Fixtures fixtures) = Vector.null fixtures

readExecutionPlan :: Maybe Command -> Test -> Execution TestExecutionPlan
readExecutionPlan defaultCommand test = do
  (executable@(Executable executableName), args) <-
    splitCommand (testCommand test <|> defaultCommand) (testArgs test)
  executableExists <- liftIO (doesFileExist executableName)
  unless executableExists $
    onNothingThrow_ (NonExistentCommand executable) =<<
    liftIO (findExecutable executableName)
  stdIn <- liftIO $ sequence $ readFixture <$> testStdIn test
  return $ TestExecutionPlan test executable args stdIn

splitCommand :: Maybe Command -> Maybe Args -> Execution (Executable, Args)
splitCommand maybeCommand maybeArgs = do
  (executableName:commandArgs) <-
    onNothingThrow NoCommand (unCommand <$> maybeCommand)
  let args = commandArgs ++ maybe [] unArgs maybeArgs
  return (Executable executableName, Args args)

readExpectedOutputs :: Test -> IO ExpectedOutputs
readExpectedOutputs test = do
  expectedStatus <- readFixture (testStatus test)
  expectedStdOuts <-
    ifEmpty (StdOut Text.empty) <$> readFixtures (testStdOut test)
  expectedStdErrs <-
    ifEmpty (StdErr Text.empty) <$> readFixtures (testStdErr test)
  return (expectedStatus, expectedStdOuts, expectedStdErrs)

executeTest :: TestExecutionPlan -> Execution ActualOutputs
executeTest (TestExecutionPlan _ executable@(Executable executableName) (Args args) stdIn) = do
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (handleExecutionError executable) $
    ExceptT $
    tryIOError $ readProcessWithExitCode executableName args processStdIn
  return (convertExitCode exitCode, StdOut processStdOut, StdErr processStdErr)
  where
    processStdIn = unStdIn $ fromMaybe (StdIn Text.empty) stdIn

handleExecutionError :: Executable -> IOError -> TestErrorMessage
handleExecutionError executable e =
  if isPermissionError e
    then NonExecutableCommand executable
    else CouldNotExecuteCommand executable (show e)

processOutput ::
     TestExecutionPlan -> ExpectedOutputs -> ActualOutputs -> TestResult
processOutput executionPlan@(TestExecutionPlan test _ _ _) (expectedStatus, expectedStdOuts, expectedStdErrs) (actualStatus, actualStdOut, actualStdErr) =
  if statusResult == PartSuccess &&
     stdOutResult == PartSuccess && stdErrResult == PartSuccess
    then TestSuccess name
    else TestFailure name executionPlan statusResult stdOutResult stdErrResult
  where
    name = testName test
    statusResult =
      if expectedStatus == actualStatus
        then PartSuccess
        else PartFailure (Vector.singleton expectedStatus) actualStatus
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

readFixtures :: FixtureContents a => Fixtures a -> IO (Vector a)
readFixtures (Fixtures fixtures) = mapM readFixture fixtures

ifEmpty :: a -> Vector a -> Vector a
ifEmpty value xs
  | Vector.null xs = Vector.singleton value
  | otherwise = xs

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value
