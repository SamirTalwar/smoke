module Main where

import Control.Monad (forM_, when)
import Data.Maybe (fromJust)
import Options
import System.Console.ANSI
import System.Exit
import Test.Smoke

main :: IO ()
main = do
  options <- parseOptions
  tests <- discoverTests options
  results <- runTests tests
  printResults options results
  printSummary options results
  exitAccordingTo results

printResults :: Options -> TestResults -> IO ()
printResults options = mapM_ (printResult options)

printResult :: Options -> TestResult -> IO ()
printResult options (TestSuccess test) = do
  putStrLn (testName test)
  putGreenLn options "  succeeded"
printResult options (TestFailure test actualStatus actualStdOut actualStdErr stdIn expectedStatus expectedStdOuts expectedStdErrs) = do
  putStrLn (testName test)
  forM_ (testArgs test) $ \args -> do
    putRed options "  args:             "
    putRedLn options (unlines $ indentedLines outputIndentation args)
  forM_ stdIn $ \input -> do
    putRed options "  input:            "
    putRedLn options (indented outputIndentation input)
  when (actualStatus /= expectedStatus) $ do
    putRed options "  actual status:    "
    putRedLn options (show actualStatus)
    putRed options "  expected status:  "
    putRedLn options (show expectedStatus)
  when (actualStdOut `notElem` expectedStdOuts) $ do
    putRed options "  actual output:    "
    putRedLn options (indented outputIndentation actualStdOut)
    putRed options "  expected output:  "
    putRedLn options (indented outputIndentation (head expectedStdOuts))
    forM_ (tail expectedStdOuts) $ \output -> do
      putRed options "               or:  "
      putRedLn options (indented outputIndentation output)
  when (actualStdErr `notElem` expectedStdErrs) $ do
    putRed options "  actual error:     "
    putRedLn options (indented outputIndentation actualStdErr)
    putRed options "  expected error:   "
    putRedLn options (indented outputIndentation (head expectedStdErrs))
    forM_ (tail expectedStdErrs) $ \output -> do
      putRed options "              or:   "
      putRedLn options (indented outputIndentation output)
printResult options (TestError test NoCommandFile) = do
  putStrLn (testName test)
  putRedLn options $ indentedAll messageIndentation "There is no command file."
printResult options (TestError test NoInputFiles) = do
  putStrLn (testName test)
  putRedLn options $
    indentedAll messageIndentation "There are no args or STDIN files."
printResult options (TestError test NoOutputFiles) = do
  putStrLn (testName test)
  putRedLn options $
    indentedAll messageIndentation "There are no STDOUT or STDERR files."
printResult options (TestError test NonExistentCommand) = do
  putStrLn (testName test)
  putRedLn options $
    indentedAll messageIndentation $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" does not exist."
printResult options (TestError test NonExecutableCommand) = do
  putStrLn (testName test)
  putRedLn options $
    indentedAll messageIndentation $
    "The application \"" ++
    unwords (fromJust (testCommand test)) ++ "\" is not executable."
printResult options (TestError test (CouldNotExecuteCommand e)) = do
  putStrLn (testName test)
  putRedLn options $ unlines $
    indentedAllLines
      messageIndentation
      [ "The application \"" ++
        unwords (fromJust (testCommand test)) ++ "\" could not be executed."
      , e
      ]

printSummary :: Options -> TestResults -> IO ()
printSummary options results = do
  putStrLn ""
  let testCount = length results
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn options (show testCount ++ " tests, 0 failures")
    1 -> putRedLn options (show testCount ++ " tests, 1 failure")
    n ->
      putRedLn options (show testCount ++ " tests, " ++ show n ++ " failures")
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

putGreen :: Options -> String -> IO ()
putGreen options = putColor options Green

putGreenLn :: Options -> String -> IO ()
putGreenLn options = putColorLn options Green

putRed :: Options -> String -> IO ()
putRed options = putColor options Red

putRedLn :: Options -> String -> IO ()
putRedLn options = putColorLn options Red

putColor :: Options -> Color -> String -> IO ()
putColor options color string =
  if optionsColor options && '\ESC' `notElem` string
    then do
      setSGR [SetColor Foreground Dull color]
      putStr string
      setSGR [Reset]
    else putStr string

putColorLn :: Options -> Color -> String -> IO ()
putColorLn options color string
  | string == "" = putStrLn ""
  | last string == '\n' = putColorLn options color (init string)
  | otherwise = do
    putColor options color string
    putStrLn ""

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
