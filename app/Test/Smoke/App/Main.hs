{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.List as List
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
      results <- runTests tests
      case optionsMode options of
        Check -> outputResults options results
        Bless -> outputResults options =<< blessResults results) `catch`
    handleDiscoveryError options

outputResults :: AppOptions -> Results -> IO ()
outputResults options results = do
  flip runReaderT options $ do
    printResults results
    printSummary results
  exitAccordingTo results

printResults :: Results -> Output ()
printResults results =
  forM_ results $ \(SuiteResult thisSuiteName testResults) ->
    let suiteNameForPrinting =
          if showSuiteNames
            then Just thisSuiteName
            else Nothing
     in forM_ testResults (printResult suiteNameForPrinting)
  where
    uniqueSuiteNames = List.nub $ map suiteResultSuiteName results
    showSuiteNames = length uniqueSuiteNames > 1

printResult :: Maybe SuiteName -> TestResult -> Output ()
printResult thisSuiteName (TestSuccess thisTestName) = do
  printTitle thisSuiteName thisTestName
  putGreenLn "  succeeded"
printResult thisSuiteName (TestFailure thisTestName (TestExecutionPlan test _ _ stdIn) statusResult stdOutResult stdErrResult) = do
  printTitle thisSuiteName thisTestName
  printFailingInput
    "args"
    (Text.unlines . map fromString . unArgs <$> testArgs test)
  printFailingInput "input" (unStdIn <$> stdIn)
  printFailingOutput "status" ((<> "\n") . int . unStatus <$> statusResult)
  printFailingOutput "output" (unStdOut <$> stdOutResult)
  printFailingOutput "error" (unStdErr <$> stdErrResult)
printResult thisSuiteName (TestError thisTestName NoCommand) = do
  printTitle thisSuiteName thisTestName
  printError "There is no command."
printResult thisSuiteName (TestError thisTestName NoInput) = do
  printTitle thisSuiteName thisTestName
  printError "There are no args or STDIN values in the specification."
printResult thisSuiteName (TestError thisTestName NoOutput) = do
  printTitle thisSuiteName thisTestName
  printError "There are no STDOUT or STDERR values in the specification."
printResult thisSuiteName (TestError thisTestName (NonExistentCommand (Executable executableName))) = do
  printTitle thisSuiteName thisTestName
  printError $
    "The application \"" <> fromString executableName <> "\" does not exist."
printResult thisSuiteName (TestError thisTestName (NonExecutableCommand (Executable executableName))) = do
  printTitle thisSuiteName thisTestName
  printError $
    "The application \"" <> fromString executableName <> "\" is not executable."
printResult thisSuiteName (TestError thisTestName (CouldNotExecuteCommand (Executable executableName) e)) = do
  printTitle thisSuiteName thisTestName
  printError $
    "The application \"" <> fromString executableName <>
    "\" could not be executed.\n" <>
    fromString e
printResult thisSuiteName (TestError thisTestName (BlessError (CouldNotWriteFixture fixtureName fixtureValue))) = do
  printTitle thisSuiteName thisTestName
  printError $
    "Could not write the fixture \"" <> fromString fixtureName <> "\":\n" <>
    fixtureValue
printResult thisSuiteName (TestError thisTestName (BlessError (CouldNotBlessAMissingValue propertyName))) = do
  printTitle thisSuiteName thisTestName
  printError $
    "There are no expected \"" <> fromString propertyName <>
    "\" values, so the result cannot be blessed.\n"
printResult thisSuiteName (TestError thisTestName (BlessError (CouldNotBlessWithMultipleValues propertyName))) = do
  printTitle thisSuiteName thisTestName
  printError $
    "There are multiple expected \"" <> fromString propertyName <>
    "\" values, so the result cannot be blessed.\n"
printResult thisSuiteName (TestError thisTestName (BlessIOException e)) = do
  printTitle thisSuiteName thisTestName
  printError $ "Blessing failed.\n" <> fromString (displayException e)

printTitle :: Maybe SuiteName -> TestName -> Output ()
printTitle (Just (SuiteName thisSuiteName)) (TestName thisTestName) =
  liftIO $ putStrLn $ thisSuiteName <> "/" <> thisTestName
printTitle Nothing (TestName thisTestName) = liftIO $ putStrLn thisTestName

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

printSummary :: Results -> Output ()
printSummary results = do
  putEmptyLn
  let testCount = length allTestResults
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn (int testCount <> " tests, 0 failures")
    1 -> putRedLn (int testCount <> " tests, 1 failure")
    n -> putRedLn (int testCount <> " tests, " <> int n <> " failures")
  where
    allTestResults = concatMap suiteResultTestResults results
    failures = filter isFailure allTestResults

printError :: Contents -> Output ()
printError = putRedLn . indentedAll messageIndentation

handleDiscoveryError :: AppOptions -> TestDiscoveryErrorMessage -> IO ()
handleDiscoveryError options e = do
  flip runReaderT options $
    putError $
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
  exitWith (ExitFailure 2)

outputIndentation :: Int
outputIndentation = 10

messageIndentation :: Int
messageIndentation = 2

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")

exitAccordingTo :: Results -> IO ()
exitAccordingTo results =
  if failureCount == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
  where
    allTestResults =
      concatMap (\(SuiteResult _ testResults) -> testResults) results
    failureCount = length (filter isFailure allTestResults)

isFailure :: TestResult -> Bool
isFailure TestSuccess {} = False
isFailure TestFailure {} = True
isFailure TestError {} = True
