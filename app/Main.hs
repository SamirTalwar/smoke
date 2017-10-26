module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (fromJust)
import Options
import System.Console.ANSI
import System.Exit
import Test.Smoke
import Text.Printf (printf)

type Output a = ReaderT Options IO a

main :: IO ()
main = do
  options <- parseOptions
  tests <- discoverTests options
  results <- runTests tests
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
  printFailingInput "args" (unlines <$> testArgs test)
  printFailingInput "input" stdIn
  printFailingOutput "status" (show <$> statusResult)
  printFailingOutput "output" stdOutResult
  printFailingOutput "error" stdErrResult
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

printTitle :: String -> Output ()
printTitle = liftIO . putStrLn

printFailingInput :: Foldable f => String -> f String -> Output ()
printFailingInput name value =
  forM_ value $ \v -> do
    putRed (printf "%-20s" ("  " ++ name ++ ":"))
    putRedLn (indented outputIndentation v)

printFailingOutput :: String -> PartResult String -> Output ()
printFailingOutput _ PartSuccess = return ()
printFailingOutput name (PartFailure expected actual) = do
  putRed (printf "%-20s" ("  actual " ++ name ++ ":"))
  putRedLn (indented outputIndentation actual)
  putRed (printf "%-20s" ("  expected " ++ name ++ ":"))
  putRedLn (indented outputIndentation (head expected))
  forM_ (tail expected) $ \output -> do
    putRed "               or:  "
    putRedLn (indented outputIndentation output)

printError :: String -> Output ()
printError = putRedLn . indentedAll messageIndentation

printSummary :: TestResults -> Output ()
printSummary results = do
  putLn
  let testCount = length results
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn (show testCount ++ " tests, 0 failures")
    1 -> putRedLn (show testCount ++ " tests, 1 failure")
    n -> putRedLn (show testCount ++ " tests, " ++ show n ++ " failures")
  where
    failures = filter isFailure results

outputIndentation :: Int
outputIndentation = 20

messageIndentation :: Int
messageIndentation = 2

indented :: Int -> String -> String
indented n = unlines . indentedLines n . lines

indentedAll :: Int -> String -> String
indentedAll n = unlines . indentedAllLines n . lines

indentedLines :: Int -> [String] -> [String]
indentedLines _ [] = []
indentedLines n (first:rest) = first : indentedAllLines n rest

indentedAllLines :: Int -> [String] -> [String]
indentedAllLines n = map (replicate n ' ' ++)

putLn :: Output ()
putLn = liftIO $ putStrLn ""

putGreen :: String -> Output ()
putGreen = putColor Green

putGreenLn :: String -> Output ()
putGreenLn = putColorLn Green

putRed :: String -> Output ()
putRed = putColor Red

putRedLn :: String -> Output ()
putRedLn = putColorLn Red

putColor :: Color -> String -> Output ()
putColor color string = do
  options <- ask
  if optionsColor options && '\ESC' `notElem` string
    then do
      liftIO $ setSGR [SetColor Foreground Dull color]
      liftIO $ putStr string
      liftIO $ setSGR [Reset]
    else liftIO $ putStr string

putColorLn :: Color -> String -> Output ()
putColorLn color string
  | string == "" = putLn
  | last string == '\n' = putColorLn color (init string)
  | otherwise = do
    putColor color string
    putLn

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
