{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Path
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
printResult (TestResult test (TestFailure testPlan statusResult stdOutResult stdErrResult fileResults)) = do
  printFailingInput
    "args"
    (Text.unlines . map fromString . unArgs <$> testArgs test)
  printFailingInput "input" (unStdIn <$> (planStdIn testPlan <$ testStdIn test))
  printFailingOutput "status" ((<> "\n") . showInt . unStatus <$> statusResult)
  printFailingOutput "stdout" (unStdOut <$> stdOutResult)
  printFailingOutput "stderr" (unStdErr <$> stdErrResult)
  printFailingFilesOutput fileResults
printResult (TestResult _ (TestError (DiscoveryError discoveryError))) = do
  options <- ask
  liftIO $ printDiscoveryError printError options discoveryError
printResult (TestResult _ (TestError (PlanningError NoCommand))) =
  printError "There is no command."
printResult (TestResult _ (TestError (PlanningError NoInput))) =
  printError "There are no args or STDIN values in the specification."
printResult (TestResult _ (TestError (PlanningError NoOutput))) =
  printError
    "There are no STDOUT or STDERR values, or files, in the specification."
printResult (TestResult _ (TestError (PlanningError (NonExistentCommand executable)))) =
  printError $ showExecutable executable <> " does not exist."
printResult (TestResult _ (TestError (PlanningError (NonExistentFixture path)))) =
  printError $ "The fixture " <> showPath path <> " does not exist."
printResult (TestResult _ (TestError (PlanningError (CouldNotReadFixture path e)))) =
  printErrorWithException e $
  "The fixture " <> showPath path <> " could not be read."
printResult (TestResult _ (TestError (PlanningError (PlanningFilterError filterError)))) =
  printFilterError filterError
printResult (TestResult _ (TestError (ExecutionError (NonExistentWorkingDirectory (WorkingDirectory path))))) =
  printError $ "The working directory " <> showPath path <> " does not exist."
printResult (TestResult _ (TestError (ExecutionError (NonExecutableCommand executable)))) =
  printError $ showExecutable executable <> " is not executable."
printResult (TestResult _ (TestError (ExecutionError (CouldNotExecuteCommand executable e)))) =
  printErrorWithException e $
  showExecutable executable <> " could not be executed."
printResult (TestResult _ (TestError (ExecutionError (CouldNotReadFile path e)))) =
  printErrorWithException e $
  "The output file " <> showPath path <> " does not exist."
printResult (TestResult _ (TestError (ExecutionError (CouldNotStoreDirectory path e)))) =
  printErrorWithException e $
  "The directory " <> showPath path <> " could not be stored."
printResult (TestResult _ (TestError (ExecutionError (CouldNotRevertDirectory path e)))) =
  printErrorWithException e $
  "The directory " <> showPath path <> " could not be reverted."
printResult (TestResult _ (TestError (ExecutionError (ExecutionFilterError filterError)))) =
  printFilterError filterError
printResult (TestResult _ (TestError (BlessError (CouldNotBlessInlineFixture fixtureName' propertyValue)))) =
  printError $
  "The fixture \"" <> showText fixtureName' <>
  "\" is embedded in the test specification, so the result cannot be blessed.\nAttempted to write:\n" <>
  indentedAll messageIndentation propertyValue
printResult (TestResult _ (TestError (BlessError (CouldNotBlessAMissingValue fixtureName')))) =
  printError $
  "There are no expected \"" <> showText fixtureName' <>
  "\" values, so the result cannot be blessed.\n"
printResult (TestResult _ (TestError (BlessError (CouldNotBlessWithMultipleValues fixtureName')))) =
  printError $
  "There are multiple expected \"" <> showText fixtureName' <>
  "\" values, so the result cannot be blessed.\n"
printResult (TestResult _ (TestError (BlessError (BlessIOException e)))) =
  printErrorWithException e "Blessing failed."

printDiscoveryError ::
     (Text -> Output ()) -> AppOptions -> SmokeDiscoveryError -> IO ()
printDiscoveryError printErrorMessage options e =
  flip runReaderT options $
  printErrorMessage $
  case e of
    NoSuchLocation path ->
      "There is no such location \"" <> fromString path <> "\"."
    NoSuchTest path (TestName selectedTestName) ->
      "There is no such test \"" <> fromString selectedTestName <> "\" in " <>
      showPath path <>
      "."
    CannotSelectTestInDirectory path (TestName selectedTestName) ->
      "The test \"" <> fromString selectedTestName <>
      "\" cannot be selected from the directory " <>
      showPath path <>
      ".\n" <>
      "Tests must be selected from a single specification file."
    InvalidSpecification path message ->
      "The test specification " <> showPath path <> " is invalid:\n" <>
      indentedAll messageIndentation (fromString message)

printFilterError :: SmokeFilterError -> Output ()
printFilterError MissingFilterScript =
  printError "The filter script is missing."
printFilterError (NonExecutableFilter executable) =
  printError $ showExecutable executable <> " is not executable."
printFilterError (CouldNotExecuteFilter executable e) =
  printErrorWithException e $
  showExecutable executable <> " could not be executed."
printFilterError (ExecutionFailed executable (Status status) (StdOut stdOut) (StdErr stdErr)) =
  printError $
  showExecutable executable <> " failed with an exit status of " <>
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

printFailingFilesOutput ::
     Map (Path Rel File) (PartResult TestFileContents) -> Output ()
printFailingFilesOutput fileResults =
  if all isSuccess (Map.elems fileResults)
    then return ()
    else do
      putRedLn "  files:"
      forM_ (Map.assocs fileResults) $ \(path, fileResult) ->
        printFailingFileOutput path (unTestFileContents <$> fileResult)
  where
    isSuccess PartSuccess = True
    isSuccess (PartFailure _) = False

printFailingFileOutput :: Path Rel File -> PartResult Text -> Output ()
printFailingFileOutput _ PartSuccess = return ()
printFailingFileOutput path (PartFailure comparisons) = do
  putRedLn $ fromString ("    " ++ toFilePath path ++ ":")
  putPlain $ fromString $ indentedKey ""
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
    showInt testCount <> " " <> testWord <> ", " <> showInt failureCount <> " " <>
    failureWord
  where
    pluralize :: Int -> Text -> Text -> Text
    pluralize 1 singular _ = singular
    pluralize _ _ plural = plural

printError :: Text -> Output ()
printError = putRedLn . indentedAll messageIndentation

printErrorWithException :: IOError -> Text -> Output ()
printErrorWithException exception =
  putRedLn .
  indentedAll messageIndentation .
  (<> "\n" <> fromString (displayException exception))

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
