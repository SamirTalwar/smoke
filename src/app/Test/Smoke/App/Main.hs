module Main
  ( main,
    run,
  )
where

import Control.Exception (catch)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Set qualified as Set
import Data.Version qualified
import Paths_smoke qualified
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
  case options of
    ShowVersionText ->
      putStrLn $ "Smoke v" <> Data.Version.showVersion Paths_smoke.version
    ShowVersionNumeric ->
      putStrLn $ Data.Version.showVersion Paths_smoke.version
    InitAppOptions appOptions ->
      run appOptions `catch` \discoveryError -> do
        withOptions appOptions $ printDiscoveryError putError discoveryError
        exitWith (ExitFailure 2)

run :: AppOptions -> IO ()
run options = do
  tests@(TestSpecification _ suites) <- discoverTests (optionsExecution options)
  let suiteNames = Set.fromList $ map suiteMetaName suites
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
  pure $ SuiteResultError suiteName suiteError
runSuite showSuiteNames (SuitePlan suiteName location testPlans) = do
  testResults <-
    forM testPlans $ runTestPlanOutcome showSuiteNames suiteName location
  pure $ SuiteResult suiteName location testResults

runTestPlanOutcome ::
  ShowSuiteNames ->
  SuiteName ->
  Path Resolved Dir ->
  TestPlanOutcome ->
  Output TestResult
runTestPlanOutcome showSuiteNames suiteName _ (TestPlanError test planningError) = do
  printTitle showSuiteNames suiteName (Just (testName test))
  let testError = PlanningError planningError
  printTestError testError
  pure $ TestErrored test testError
runTestPlanOutcome showSuiteNames suiteName location (TestPlanSuccess testPlan@TestPlan {planTest = test}) = do
  printTitle showSuiteNames suiteName (Just (testName test))
  runTestPlan location testPlan

runTestPlan :: Path Resolved Dir -> TestPlan -> Output TestResult
runTestPlan location testPlan = do
  AppOptions {optionsMode = mode} <- ask
  executionResult <- liftIO $ runTest location testPlan
  testResult <- liftIO $ assertResult location testPlan executionResult
  modifiedTestResult <-
    case mode of
      Check -> pure testResult
      Bless -> liftIO $ blessResult location testResult
  printResult modifiedTestResult
  pure modifiedTestResult

withOptions :: AppOptions -> ReaderT AppOptions m a -> m a
withOptions = flip runReaderT
