module Main
  ( main,
  )
where

import Control.Exception (catch)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.Set as Set
import System.Exit
import Test.Smoke
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Options
import Test.Smoke.App.Print
import Test.Smoke.App.PrintErrors
import Test.Smoke.App.PrintResults
import Test.Smoke.App.PrintSummary
import Test.Smoke.App.PrintTitle
import Test.Smoke.Paths

main :: IO ()
main = do
  options <- parseOptions
  run options `catch` \discoveryError -> do
    withOptions options $ printDiscoveryError putError discoveryError
    exitWith (ExitFailure 2)

run :: AppOptions -> IO ()
run options = do
  tests@(TestSpecification _ suites) <- discoverTests (optionsExecution options)
  let suiteNames = Set.fromList $ map fst suites
  let showSuiteNames = Set.size suiteNames > 1
  (Plan plannedSuites) <- planTests tests
  results <- withOptions options $ forM plannedSuites $ runSuite showSuiteNames
  let summary = summarizeResults results
  withOptions options $ printSummary summary
  if summaryFailures summary == 0
    then exitSuccess
    else exitWith (ExitFailure 1)

runSuite :: ShowSuiteNames -> SuitePlan -> Output SuiteResult
runSuite showSuiteNames (SuitePlanError suiteName suiteError) = do
  printTitle showSuiteNames suiteName Nothing
  printSuiteError suiteError
  return $ SuiteResultError suiteName suiteError
runSuite showSuiteNames (SuitePlan suiteName location testPlans) = do
  testResults <-
    forM testPlans $ runTestPlanOutcome showSuiteNames suiteName location
  return $ SuiteResult suiteName location testResults

runTestPlanOutcome ::
  ShowSuiteNames ->
  SuiteName ->
  ResolvedPath Dir ->
  TestPlanOutcome ->
  Output TestResult
runTestPlanOutcome showSuiteNames suiteName _ (TestPlanError test planningError) = do
  printTitle showSuiteNames suiteName (Just (testName test))
  let testError = PlanningError planningError
  printTestError testError
  return $ TestResult test $ TestError testError
runTestPlanOutcome showSuiteNames suiteName location (TestPlanSuccess testPlan@TestPlan {planTest = test}) = do
  printTitle showSuiteNames suiteName (Just (testName test))
  runTestPlan location testPlan

runTestPlan :: ResolvedPath Dir -> TestPlan -> Output TestResult
runTestPlan location testPlan = do
  let test = planTest testPlan
  AppOptions {optionsMode = mode} <- ask
  executionResult <- liftIO $ runTest location testPlan
  testOutcome <- liftIO $ assertResult location testPlan executionResult
  let testResult = TestResult test testOutcome
  modifiedTestResult <-
    case mode of
      Check -> return testResult
      Bless -> liftIO $ blessResult location testResult
  printResult modifiedTestResult
  return modifiedTestResult

withOptions :: AppOptions -> ReaderT AppOptions m a -> m a
withOptions = flip runReaderT
