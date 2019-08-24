{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.PrintResults
  ( outputResults
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Path
import Test.Smoke
import Test.Smoke.App.Diff
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Print
import Test.Smoke.App.PrintErrors
import Text.Printf (printf)

outputResults :: Results -> Output Summary
outputResults results = do
  let summary = summarizeResults results
  printResults results
  printSummary summary
  return summary

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
      if showSuiteNames || Maybe.isNothing thisTestName
        then Just thisSuiteName
        else Nothing
    name =
      List.intercalate "/" $
      Maybe.catMaybes
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

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")
