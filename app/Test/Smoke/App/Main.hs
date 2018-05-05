{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.String (fromString)
import System.Console.ANSI
import System.Exit
import Test.Smoke
import Test.Smoke.App.Diff
import Test.Smoke.App.Options
import Test.Smoke.App.Print
import Text.Printf (printf)

type Output a = ReaderT AppOptions IO a

main :: IO ()
main = do
  options <- parseOptions
  tests <- discoverTests (optionsExecution options)
  results <- runTests tests
  if optionsBless options
    then outputResults options =<< blessResults results
    else outputResults options results

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
  printFailingInput "args" (fromString . unlines <$> testArgs test)
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
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" does not exist."
printResult (TestError test NonExecutableCommand) = do
  printTitle (testName test)
  printError $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" is not executable."
printResult (TestError test (CouldNotExecuteCommand e)) = do
  printTitle (testName test)
  printError $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" could not be executed.\n" ++ e
printResult (TestError test (BlessingFailed e)) = do
  printTitle (testName test)
  printError $ "Blessing failed.\n" ++ displayException e
printResult (TestError test CouldNotBlessStdOutWithMultipleValues) = do
  printTitle (testName test)
  printError
    "There are multiple expected STDOUT values, so the result cannot be blessed.\n"
printResult (TestError test CouldNotBlessStdErrWithMultipleValues) = do
  printTitle (testName test)
  printError
    "There are multiple expected STDERR values, so the result cannot be blessed.\n"

printTitle :: String -> Output ()
printTitle = liftIO . putStrLn

printFailingInput :: Foldable f => String -> f OutputString -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
    putPlainLn $ indented outputIndentation v

printFailingOutput :: String -> PartResult OutputString -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure expected actual) = do
  putRed $ fromString $ indentedKey ("  " ++ name ++ ":")
  putDiff (head expected) actual
  forM_ (tail expected) $ \e -> do
    putRed "      or: "
    putDiff e actual

printError :: String -> Output ()
printError = putRedLn . indentedAll messageIndentation . fromString

printSummary :: TestResults -> Output ()
printSummary results = do
  printEmptyLn
  let testCount = length results
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn (int testCount <> " tests, 0 failures")
    1 -> putRedLn (int testCount <> " tests, 1 failure")
    n -> putRedLn (int testCount <> " tests, " <> int n <> " failures")
  where
    failures = filter isFailure results

outputIndentation :: Int
outputIndentation = 10

messageIndentation :: Int
messageIndentation = 2

indentedKey :: String -> String
indentedKey = printf ("%-" ++ show outputIndentation ++ "s")

printEmptyLn :: Output ()
printEmptyLn = liftIO $ putStrLn ""

putDiff :: OutputString -> OutputString -> Output ()
putDiff left right =
  putPlainLn $ indented outputIndentation $ prettyPrintDiff left right

putPlainLn :: OutputString -> Output ()
putPlainLn string = do
  liftIO $ printStr $ stripTrailingNewline string
  printEmptyLn

putGreen :: OutputString -> Output ()
putGreen = putColor Green

putGreenLn :: OutputString -> Output ()
putGreenLn = putColorLn Green

putRed :: OutputString -> Output ()
putRed = putColor Red

putRedLn :: OutputString -> Output ()
putRedLn = putColorLn Red

putColor :: Color -> OutputString -> Output ()
putColor color string = do
  options <- ask
  if optionsColor options && not (hasEsc string)
    then do
      liftIO $ setSGR [SetColor Foreground Dull color]
      liftIO $ printStr string
      liftIO $ setSGR [Reset]
    else liftIO $ printStr string

putColorLn :: Color -> OutputString -> Output ()
putColorLn color string = do
  putColor color (stripTrailingNewline string)
  printEmptyLn

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
