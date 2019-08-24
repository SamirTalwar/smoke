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
  run options `catch` \discoveryError -> do
    flip runReaderT options $ printDiscoveryError putError discoveryError
    exitWith (ExitFailure 2)

run :: AppOptions -> IO ()
run options = do
  tests <- discoverTests (optionsExecution options)
  plan <- planTests tests
  results <- runTests plan
  case optionsMode options of
    Check -> outputResults options results
    Bless -> outputResults options =<< blessResults results

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
    SuiteResultDiscoveryError suiteName discoveryError -> do
      printTitle showSuiteNames suiteName Nothing
      printDiscoveryError printError discoveryError
    SuiteResultExecutableError suiteName executableError -> do
      printTitle showSuiteNames suiteName Nothing
      printExecutableError executableError
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
    (Text.unlines . Vector.toList . Vector.map fromString . unArgs <$>
     testArgs test)
  printFailingInput "input" (unStdIn <$> (planStdIn testPlan <$ testStdIn test))
  printFailingOutput "status" ((<> "\n") . showInt . unStatus <$> statusResult)
  printFailingOutput "stdout" (unStdOut <$> stdOutResult)
  printFailingOutput "stderr" (unStdErr <$> stdErrResult)
  printFailingFilesOutput fileResults
printResult (TestResult _ (TestError testError)) = printTestError testError

printTestError :: SmokeError -> Output ()
printTestError (DiscoveryError discoveryError) =
  printDiscoveryError printError discoveryError
printTestError (PlanningError NoCommand) = printError "There is no command."
printTestError (PlanningError NoInput) =
  printError "There are no args or STDIN values in the specification."
printTestError (PlanningError NoOutput) =
  printError
    "There are no STDOUT or STDERR values, or files, in the specification."
printTestError (PlanningError (NonExistentFixture path)) =
  printError $ "The fixture " <> showPath path <> " does not exist."
printTestError (PlanningError (CouldNotReadFixture path exception)) =
  printErrorWithException exception $
  "The fixture " <> showPath path <> " could not be read."
printTestError (PlanningError (PlanningFilterError filterError)) =
  printFilterError filterError
printTestError (PlanningError (PlanningExecutableError executableError)) =
  printExecutableError executableError
printTestError (ExecutionError (NonExistentWorkingDirectory (WorkingDirectory path))) =
  printError $ "The working directory " <> showPath path <> " does not exist."
printTestError (ExecutionError (CouldNotExecuteCommand executable exception)) =
  printErrorWithException exception $
  showExecutable executable <> " could not be executed."
printTestError (ExecutionError (CouldNotReadFile path exception)) =
  printErrorWithException exception $
  "The output file " <> showPath path <> " does not exist."
printTestError (ExecutionError (CouldNotStoreDirectory path exception)) =
  printErrorWithException exception $
  "The directory " <> showPath path <> " could not be stored."
printTestError (ExecutionError (CouldNotRevertDirectory path exception)) =
  printErrorWithException exception $
  "The directory " <> showPath path <> " could not be reverted."
printTestError (ExecutionError (ExecutionFilterError filterError)) =
  printFilterError filterError
printTestError (BlessError (CouldNotBlessInlineFixture fixtureName' propertyValue)) =
  printError $
  "The fixture \"" <> showText fixtureName' <>
  "\" is embedded in the test specification, so the result cannot be blessed.\nAttempted to write:\n" <>
  indentedAll messageIndentation propertyValue
printTestError (BlessError (CouldNotBlessAMissingValue fixtureName')) =
  printError $
  "There are no expected \"" <> showText fixtureName' <>
  "\" values, so the result cannot be blessed.\n"
printTestError (BlessError (CouldNotBlessWithMultipleValues fixtureName')) =
  printError $
  "There are multiple expected \"" <> showText fixtureName' <>
  "\" values, so the result cannot be blessed.\n"
printTestError (BlessError (BlessIOException exception)) =
  printErrorWithException exception "Blessing failed."

printDiscoveryError :: (Text -> Output ()) -> SmokeDiscoveryError -> Output ()
printDiscoveryError printErrorMessage = printErrorMessage . printDiscoveryError'
  where
    printDiscoveryError' :: SmokeDiscoveryError -> Text
    printDiscoveryError' (NoSuchLocation path) =
      "There is no such location \"" <> fromString path <> "\"."
    printDiscoveryError' (NoSuchTest path (TestName selectedTestName)) =
      "There is no such test \"" <> fromString selectedTestName <> "\" in " <>
      showPath path <>
      "."
    printDiscoveryError' (CannotSelectTestInDirectory path (TestName selectedTestName)) =
      "The test \"" <> fromString selectedTestName <>
      "\" cannot be selected from the directory " <>
      showPath path <>
      ".\n" <>
      "Tests must be selected from a single specification file."
    printDiscoveryError' (InvalidSpecification path message) =
      "The test specification " <> showPath path <> " is invalid:\n" <>
      indentedAll messageIndentation (fromString message)

printFilterError :: SmokeFilterError -> Output ()
printFilterError MissingFilterScript =
  printError "The filter script is missing."
printFilterError (CouldNotExecuteFilter executable exception) =
  printErrorWithException exception $
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
printFilterError (FilterExecutableError executableError) =
  printExecutableError executableError

printExecutableError :: SmokeExecutableError -> Output ()
printExecutableError (CouldNotFindExecutable path) =
  printError $
  "The executable \"" <> fromString path <> "\" could not be found."
printExecutableError (FileIsNotExecutable path) =
  printError $ "The file at \"" <> fromString path <> "\" is not executable."

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
