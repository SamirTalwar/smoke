{-# LANGUAGE LambdaCase #-}

module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import qualified Data.Vector as Vector
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.Errors
import Test.Smoke.Types

type Execution = ExceptT TestErrorMessage IO

type ActualOutputs = (Status, StdOut, StdErr)

runTests :: Plan -> IO Results
runTests (Plan suites) =
  forM suites $ \(suiteName, suitePlans) ->
    SuiteResult suiteName <$>
    case suitePlans of
      Left errorMessage -> return $ Left errorMessage
      Right testPlans -> do
        testResults <-
          forM testPlans $ \case
            Left (TestPlanError test errorMessage) ->
              return $ TestResult test $ TestError $ PlanError errorMessage
            Right testPlan -> runTest testPlan
        return $ Right testResults

runTest :: TestPlan -> IO TestResult
runTest testPlan =
  handleError (TestResult (planTest testPlan) . TestError) <$>
  runExceptT
    (do actualOutput <- executeTest testPlan
        return $ processOutput testPlan actualOutput)

executeTest :: TestPlan -> Execution ActualOutputs
executeTest (TestPlan _ executable@(Executable executableName) (Args args) stdIn _ _ _) = do
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (handleExecutionError executable) $
    ExceptT $
    tryIOError $ readProcessWithExitCode executableName args processStdIn
  return (convertExitCode exitCode, StdOut processStdOut, StdErr processStdErr)
  where
    processStdIn = unStdIn stdIn

handleExecutionError :: Executable -> IOError -> TestErrorMessage
handleExecutionError executable e =
  if isPermissionError e
    then NonExecutableCommand executable
    else CouldNotExecuteCommand executable (show e)

processOutput :: TestPlan -> ActualOutputs -> TestResult
processOutput testPlan@(TestPlan test _ _ _ expectedStatus expectedStdOuts expectedStdErrs) (actualStatus, actualStdOut, actualStdErr) =
  TestResult test $
  if statusResult == PartSuccess &&
     stdOutResult == PartSuccess && stdErrResult == PartSuccess
    then TestSuccess
    else TestFailure testPlan statusResult stdOutResult stdErrResult
  where
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

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value
