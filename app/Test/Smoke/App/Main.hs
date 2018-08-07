{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as Text
import System.Exit
import Test.Smoke
import Test.Smoke.App.Diff
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Options
import Test.Smoke.App.Print
import Text.Printf (printf)

main :: IO ()
main = do
  options <- parseOptions
  tests <- discoverTests (optionsExecution options)
  results <- runTests tests
  case optionsMode options of
    Check -> outputResults options results
    Bless -> outputResults options =<< blessResults results

outputResults :: AppOptions -> TestResults -> IO ()
outputResults options results = do
  runReaderT
    (do printResults results
        printSummary results)
    options
  exitAccordingTo results

printResults :: TestResults -> Output ()
printResults = mapM_ printResult

printResult :: TestResult -> Output ()
printResult (TestSuccess test) = do
  printTitle (testName test)
  putGreenLn "  succeeded"
printResult (TestFailure (TestExecutionPlan test _ _ stdIn) statusResult stdOutResult stdErrResult) = do
  printTitle (testName test)
  printFailingInput "args" (Text.unlines . map fromString <$> testArgs test)
  printFailingInput "input" (unStdIn <$> stdIn)
  printFailingOutput "status" (int . unStatus <$> statusResult)
  printFailingOutput "output" (unStdOut <$> stdOutResult)
  printFailingOutput "error" (unStdErr <$> stdErrResult)
printResult (TestError test NoCommandFile) = do
  printTitle (testName test)
  printError "There is no command file."
printResult (TestError test NoInputFiles) = do
  printTitle (testName test)
  printError "There are no args or STDIN files."
printResult (TestError test NoOutputFiles) = do
  printTitle (testName test)
  printError "There are no STDOUT or STDERR files."
printResult (TestError test NonExistentCommand) = do
  printTitle (testName test)
  printError $
    "The application \"" <>
    Text.unwords (map fromString $ fromJust $ testCommand test) <>
    "\" does not exist."
printResult (TestError test NonExecutableCommand) = do
  printTitle (testName test)
  printError $
    "The application \"" <>
    Text.unwords (map fromString $ fromJust $ testCommand test) <>
    "\" is not executable."
printResult (TestError test (CouldNotExecuteCommand e)) = do
  printTitle (testName test)
  printError $
    "The application \"" <>
    Text.unwords (map fromString $ fromJust $ testCommand test) <>
    "\" could not be executed.\n" <>
    fromString e
printResult (TestError test (CouldNotWriteFixture name value)) = do
  printTitle (testName test)
  printError $
    "Could not write the fixture \"" <> Text.pack name <> "\":\n" <> value
printResult (TestError test (BlessingFailed e)) = do
  printTitle (testName test)
  printError $ "Blessing failed.\n" <> fromString (displayException e)
printResult (TestError test (CouldNotBlessAMissingValue propertyName)) = do
  printTitle (testName test)
  printError $
    "There are no expected \"" <> Text.pack propertyName <>
    "\" values, so the result cannot be blessed.\n"
printResult (TestError test (CouldNotBlessWithMultipleValues propertyName)) = do
  printTitle (testName test)
  printError $
    "There are multiple expected \"" <> Text.pack propertyName <>
    "\" values, so the result cannot be blessed.\n"

printTitle :: String -> Output ()
printTitle = liftIO . putStrLn

printFailingInput :: Foldable f => String -> f Contents -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
    putPlainLn $ indented outputIndentation v

printFailingOutput :: String -> PartResult Contents -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure expected actual) = do
  putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
  printDiff (head expected) actual
  forM_ (tail expected) $ \e -> do
    putRed "      or: "
    printDiff e actual

printSummary :: TestResults -> Output ()
printSummary results = do
  putEmptyLn
  let testCount = length results
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn (int testCount <> " tests, 0 failures")
    1 -> putRedLn (int testCount <> " tests, 1 failure")
    n -> putRedLn (int testCount <> " tests, " <> int n <> " failures")
  where
    failures = filter isFailure results

printError :: Contents -> Output ()
printError = putRedLn . indentedAll messageIndentation

outputIndentation :: Int
outputIndentation = 10

messageIndentation :: Int
messageIndentation = 2

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")

printDiff :: Contents -> Contents -> Output ()
printDiff left right = do
  AppOptions { optionsColor = color
             , optionsDiffEngine = DiffEngine {engineRender = renderDiff}
             } <- ask
  diff <- liftIO $ renderDiff color left right
  putPlainLn $ indented outputIndentation diff

exitAccordingTo :: TestResults -> IO ()
exitAccordingTo results =
  if failureCount == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
  where
    failureCount = length (filter isFailure results)

isFailure :: TestResult -> Bool
isFailure TestSuccess {} = False
isFailure TestFailure {} = True
isFailure TestError {} = True
