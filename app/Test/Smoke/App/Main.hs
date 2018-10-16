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
    handleDiscoveryError putError options e
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
  forM_ results $ \(SuiteResult thisSuiteName thisSuiteResult) ->
    case thisSuiteResult of
      Left discoveryErrorMessage -> do
        printTitle showSuiteNames thisSuiteName Nothing
        appOptions <- ask
        liftIO $
          handleDiscoveryError printError appOptions discoveryErrorMessage
      Right testResults ->
        forM_ testResults $ \testResult@(TestResult test _) -> do
          printTitle showSuiteNames thisSuiteName (Just $ testName test)
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
  printFailingInput
    "input"
    (unStdIn <$> (const (planStdIn testPlan) <$> testStdIn test))
  printFailingOutput "status" ((<> "\n") . int . unStatus <$> statusResult)
  printFailingOutput "output" (unStdOut <$> stdOutResult)
  printFailingOutput "error" (unStdErr <$> stdErrResult)
printResult (TestResult _ (TestError (PlanError NoCommand))) =
  printError "There is no command."
printResult (TestResult _ (TestError (PlanError NoInput))) =
  printError "There are no args or STDIN values in the specification."
printResult (TestResult _ (TestError (PlanError NoOutput))) =
  printError "There are no STDOUT or STDERR values in the specification."
printResult (TestResult _ (TestError (PlanError (NonExistentCommand (Executable executableName))))) =
  printError $
  "The application \"" <> fromString executableName <> "\" does not exist."
printResult (TestResult _ (TestError (NonExecutableCommand (Executable executableName)))) =
  printError $
  "The application \"" <> fromString executableName <> "\" is not executable."
printResult (TestResult _ (TestError (CouldNotExecuteCommand (Executable executableName) e))) =
  printError $
  "The application \"" <> fromString executableName <>
  "\" could not be executed.\n" <>
  fromString e
printResult (TestResult _ (TestError (BlessError (CouldNotBlessInlineFixture propertyName propertyValue)))) =
  printError $
  "The fixture \"" <> fromString propertyName <>
  " is embedded in the test specification, so the result cannot be blessed.\nAttempted to write:\n" <>
  indentedAll messageIndentation propertyValue
printResult (TestResult _ (TestError (BlessError (CouldNotBlessAMissingValue propertyName)))) =
  printError $
  "There are no expected \"" <> fromString propertyName <>
  "\" values, so the result cannot be blessed.\n"
printResult (TestResult _ (TestError (BlessError (CouldNotBlessWithMultipleValues propertyName)))) =
  printError $
  "There are multiple expected \"" <> fromString propertyName <>
  "\" values, so the result cannot be blessed.\n"
printResult (TestResult _ (TestError (BlessIOException e))) =
  printError $
  "Blessing failed:\n" <>
  indentedAll messageIndentation (fromString (displayException e))

printFailingInput :: Foldable f => String -> f Contents -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
    putPlainLn $ indented outputIndentation v

printFailingOutput :: String -> PartResult Contents -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure expected actual) = do
  putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
  printDiff (Vector.head expected) actual
  forM_ (Vector.tail expected) $ \e -> do
    putRed "      or: "
    printDiff e actual

printDiff :: Contents -> Contents -> Output ()
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
    pluralize :: Int -> Contents -> Contents -> Contents
    pluralize 1 singular _ = singular
    pluralize _ _ plural = plural

printError :: Contents -> Output ()
printError = putRedLn . indentedAll messageIndentation

handleDiscoveryError ::
     (Contents -> Output ()) -> AppOptions -> TestDiscoveryErrorMessage -> IO ()
handleDiscoveryError printErrorMessage options e =
  flip runReaderT options $
  printErrorMessage $
  case e of
    NoSuchLocation location ->
      "There is no such location \"" <> fromString location <> "\"."
    NoSuchTest location (TestName selectedTestName) ->
      "There is no such test \"" <> fromString selectedTestName <> "\" in \"" <>
      fromString location <>
      "\"."
    CannotSelectTestInDirectory location (TestName selectedTestName) ->
      "The test \"" <> fromString selectedTestName <>
      "\" cannot be selected from the directory \"" <>
      fromString location <>
      "\".\n" <>
      "Tests must be selected from a single specification file."
    InvalidSpecification path message ->
      "The test specification \"" <> fromString path <> "\" is invalid:\n" <>
      indentedAll messageIndentation (fromString message)

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
