{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.List as List
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.Exit
import Test.Smoke
import Test.Smoke.App.Diff
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Options
import Test.Smoke.App.Print
import Text.Printf (printf)

type ShowSuiteNames = Bool

main :: IO ()
main = do
  options <- parseOptions
  (do tests <- discoverTests (optionsExecution options)
      plan <- planTests tests
      results <- runTests plan
      case optionsMode options of
        Check -> outputResults options results
        Bless -> outputResults options =<< blessResults results) `catch` \e -> do
    printDiscoveryError putError options e
    exitWith (ExitFailure 2)

outputResults :: AppOptions -> Results -> IO ()
outputResults options results = do
  let summary = summarizeResults results
  flip runReaderT options $ do
    printResults results
    printSummary summary
  exitAccordingTo summary

printResults :: Results -> Output ()
printResults results =
  forM_ results $ \case
    SuiteResultError suiteName discoveryErrorMessage -> do
      printTitle showSuiteNames suiteName Nothing
      appOptions <- ask
      liftIO $ printDiscoveryError printError appOptions discoveryErrorMessage
    SuiteResult suiteName _ testResults ->
      forM_ testResults $ \testResult@(TestResult test _) -> do
        printTitle showSuiteNames suiteName (Just $ testName test)
        printResult testResult
  where
    uniqueSuiteNames = List.nub $ map suiteResultSuiteName results
    showSuiteNames = length uniqueSuiteNames > 1

printTitle :: ShowSuiteNames -> SuiteName -> Maybe TestName -> Output ()
printTitle showSuiteNames thisSuiteName thisTestName = liftIO $ putStrLn name
  where
    suiteNameForPrinting =
      if showSuiteNames || isNothing thisTestName
        then Just thisSuiteName
        else Nothing
    name =
      List.intercalate "/" $
      catMaybes
        [unSuiteName <$> suiteNameForPrinting, unTestName <$> thisTestName]

printResult :: TestResult -> Output ()
printResult (TestResult _ TestSuccess) = putGreenLn "  succeeded"
printResult (TestResult test (TestFailure testPlan statusResult stdOutResult stdErrResult)) = do
  printFailingInput
    "args"
    (Text.unlines . map fromString . unArgs <$> testArgs test)
  printFailingInput "input" (unStdIn <$> (planStdIn testPlan <$ testStdIn test))
  printFailingOutput "status" ((<> "\n") . int . unStatus <$> statusResult)
  printFailingOutput "stdout" (unStdOut <$> stdOutResult)
  printFailingOutput "stderr" (unStdErr <$> stdErrResult)
printResult (TestResult _ (TestError (DiscoveryError discoveryError))) = do
  options <- ask
  liftIO $ printDiscoveryError printError options discoveryError
printResult (TestResult _ (TestError (PlanningError NoCommand))) =
  printError "There is no command."
printResult (TestResult _ (TestError (PlanningError NoInput))) =
  printError "There are no args or STDIN values in the specification."
printResult (TestResult _ (TestError (PlanningError NoOutput))) =
  printError "There are no STDOUT or STDERR values in the specification."
printResult (TestResult _ (TestError (PlanningError (NonExistentCommand (Executable executablePath))))) =
  printError $
  "The application \"" <> showText executablePath <> "\" does not exist."
printResult (TestResult _ (TestError (PlanningError (NonExistentFixture path)))) =
  printError $ "The fixture \"" <> showText path <> "\" does not exist."
printResult (TestResult _ (TestError (PlanningError (CouldNotReadFixture path e)))) =
  printError $
  "The fixture \"" <> showText path <> "\" could not be read.\n" <> fromString e
printResult (TestResult _ (TestError (PlanningError (PlanningFilterError filterError)))) =
  printFilterError filterError
printResult (TestResult _ (TestError (ExecutionError (NonExecutableCommand (Executable executablePath))))) =
  printError $
  "The application \"" <> showText executablePath <> "\" is not executable."
printResult (TestResult _ (TestError (ExecutionError (CouldNotExecuteCommand (Executable executablePath) e)))) =
  printError $
  "The application \"" <> showText executablePath <>
  "\" could not be executed.\n" <>
  fromString e
printResult (TestResult _ (TestError (ExecutionError (ExecutionFilterError filterError)))) =
  printFilterError filterError
printResult (TestResult _ (TestError (BlessError (CouldNotBlessInlineFixture propertyName propertyValue)))) =
  printError $
  "The fixture \"" <> fromString propertyName <>
  "\" is embedded in the test specification, so the result cannot be blessed.\nAttempted to write:\n" <>
  indentedAll messageIndentation propertyValue
printResult (TestResult _ (TestError (BlessError (CouldNotBlessAMissingValue propertyName)))) =
  printError $
  "There are no expected \"" <> fromString propertyName <>
  "\" values, so the result cannot be blessed.\n"
printResult (TestResult _ (TestError (BlessError (CouldNotBlessWithMultipleValues propertyName)))) =
  printError $
  "There are multiple expected \"" <> fromString propertyName <>
  "\" values, so the result cannot be blessed.\n"
printResult (TestResult _ (TestError (BlessError (BlessIOException e)))) =
  printError $
  "Blessing failed:\n" <>
  indentedAll messageIndentation (fromString (displayException e))

printDiscoveryError ::
     (Text -> Output ()) -> AppOptions -> SmokeDiscoveryError -> IO ()
printDiscoveryError printErrorMessage options e =
  flip runReaderT options $
  printErrorMessage $
  case e of
    NoSuchLocation path ->
      "There is no such location \"" <> showText path <> "\"."
    NoSuchTest path (TestName selectedTestName) ->
      "There is no such test \"" <> fromString selectedTestName <> "\" in \"" <>
      showText path <>
      "\"."
    CannotSelectTestInDirectory path (TestName selectedTestName) ->
      "The test \"" <> fromString selectedTestName <>
      "\" cannot be selected from the directory \"" <>
      showText path <>
      "\".\n" <>
      "Tests must be selected from a single specification file."
    InvalidSpecification path message ->
      "The test specification \"" <> showText path <> "\" is invalid:\n" <>
      indentedAll messageIndentation (fromString message)

printFilterError :: SmokeFilterError -> Output ()
printFilterError (NonExecutableFilter (Executable executablePath)) =
  printError $
  "The application \"" <> showText executablePath <> "\" is not executable."
printFilterError (CouldNotExecuteFilter (Executable executablePath) e) =
  printError $
  "The application \"" <> showText executablePath <>
  "\" could not be executed.\n" <>
  fromString e
printFilterError (ExecutionFailed (Executable executablePath) (Status status) (StdOut stdOut) (StdErr stdErr)) =
  printError $
  "The application \"" <> showText executablePath <>
  "\" failed with an exit status of " <>
  showText status <>
  "." <>
  "\nSTDOUT:\n" <>
  indentedAll messageIndentation stdOut <>
  "\nSTDERR:\n" <>
  indentedAll messageIndentation stdErr

printFailingInput :: Foldable f => String -> f Text -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
    putPlainLn $ indented outputIndentation v

printFailingOutput :: String -> PartResult Text -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure comparisons) = do
  putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
  uncurry printDiff (Vector.head comparisons)
  forM_ (Vector.tail comparisons) $ \(expected, actual) -> do
    putRed "      or: "
    printDiff expected actual

printDiff :: Text -> Text -> Output ()
printDiff left right = do
  AppOptions { optionsColor = color
             , optionsDiffEngine = DiffEngine {engineRender = renderDiff}
             } <- ask
  diff <- liftIO $ renderDiff color left right
  putPlainLn $ indented outputIndentation diff

printSummary :: Summary -> Output ()
printSummary summary = do
  putEmptyLn
  let testCount = summaryTotal summary
  let failureCount = summaryFailures summary
  let testWord = pluralize testCount "test" "tests"
  let failureWord = pluralize failureCount "failure" "failures"
  let printSummaryLine =
        if failureCount == 0
          then putGreenLn
          else putRedLn
  printSummaryLine $
    int testCount <> " " <> testWord <> ", " <> int failureCount <> " " <>
    failureWord
  where
    pluralize :: Int -> Text -> Text -> Text
    pluralize 1 singular _ = singular
    pluralize _ _ plural = plural

printError :: Text -> Output ()
printError = putRedLn . indentedAll messageIndentation

outputIndentation :: Int
outputIndentation = 10

messageIndentation :: Int
messageIndentation = 2

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")

exitAccordingTo :: Summary -> IO ()
exitAccordingTo summary =
  if summaryFailures summary == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
