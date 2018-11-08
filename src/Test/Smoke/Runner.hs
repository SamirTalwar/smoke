{-# LANGUAGE LambdaCase #-}

module Test.Smoke.Runner
  ( runTests
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Default
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.Errors
import Test.Smoke.Filters
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
  runExceptT (processOutput testPlan =<< executeTest testPlan)

executeTest :: TestPlan -> Execution ActualOutputs
executeTest (TestPlan _ executable (Args args) (StdIn processStdIn) _ _ _) = do
  let executableName = show $ unExecutable executable
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (handleExecutionError executable) $
    ExceptT $
    tryIOError $ readProcessWithExitCode executableName args processStdIn
  return (convertExitCode exitCode, StdOut processStdOut, StdErr processStdErr)

processOutput :: TestPlan -> ActualOutputs -> Execution TestResult
processOutput testPlan@(TestPlan test _ _ _ expectedStatus expectedStdOuts expectedStdErrs) (actualStatus, actualStdOut, actualStdErr) = do
  filteredStatus <-
    withExceptT FilterError $
    applyFiltersFromFixture (testStatus test) actualStatus
  filteredStdOut <-
    withExceptT FilterError $
    applyFiltersFromFixtures (testStdOut test) actualStdOut
  filteredStdErr <-
    withExceptT FilterError $
    applyFiltersFromFixtures (testStdErr test) actualStdErr
  let statusResult = result $ Vector.singleton (expectedStatus, filteredStatus)
  let stdOutResult =
        result $ defaultIfEmpty $ Vector.zip expectedStdOuts filteredStdOut
  let stdErrResult =
        result $ defaultIfEmpty $ Vector.zip expectedStdErrs filteredStdErr
  return $
    TestResult test $
    if statusResult == PartSuccess &&
       stdOutResult == PartSuccess && stdErrResult == PartSuccess
      then TestSuccess
      else TestFailure testPlan statusResult stdOutResult stdErrResult
  where
    result :: Eq a => Vector (a, a) -> PartResult a
    result comparison =
      if Vector.any (uncurry (==)) comparison
        then PartSuccess
        else PartFailure comparison

handleExecutionError :: Executable -> IOError -> TestErrorMessage
handleExecutionError executable e =
  if isPermissionError e
    then NonExecutableCommand executable
    else CouldNotExecuteCommand executable (show e)

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value

defaultIfEmpty :: Default a => Vector a -> Vector a
defaultIfEmpty xs
  | Vector.null xs = Vector.singleton def
  | otherwise = xs
