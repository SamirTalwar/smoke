module Main where

import Control.Monad (forM_, when)
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
  when (actualStatus /= expectedStatus) $ do
    putRed options "  actual status:    "
    putRedLn options (show actualStatus)
    putRed options "  expected status:  "
    putRedLn options (show expectedStatus)
  forM_ stdIn $ \input -> do
    putRed options "  input:            "
    putRedLn options input
  when (actualStdOut `notElem` expectedStdOuts) $ do
    putRed options "  actual output:    "
    putRedLn options actualStdOut
    putRed options "  expected output:  "
    putRedLn options (head expectedStdOuts)
    forM_ (tail expectedStdOuts) $ \output -> do
      putRed options "               or:  "
      putRedLn options output
  when (actualStdErr `notElem` expectedStdErrs) $ do
    putRed options "  actual error:     "
    putRedLn options actualStdErr
    putRed options "  expected error:   "
    putRedLn options (head expectedStdErrs)
    forM_ (tail expectedStdErrs) $ \output -> do
      putRed options "              or:   "
      putRedLn options output
printResult options (TestError test CouldNotFindExecutable) = do
  putStrLn (testName test)
  putRedLn options "  could not find the executable"

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

putGreen :: Options -> String -> IO ()
putGreen options = putColor options Green

putGreenLn :: Options -> String -> IO ()
putGreenLn options = putColorLn options Green

putRed :: Options -> String -> IO ()
putRed options = putColor options Red

putRedLn :: Options -> String -> IO ()
putRedLn options = putColorLn options Red

putColor :: Options -> Color -> String -> IO ()
putColor options color string = do
  when (optionsColor options) $ setSGR [SetColor Foreground Dull color]
  putStr string
  when (optionsColor options) $ setSGR [Reset]

putColorLn :: Options -> Color -> String -> IO ()
putColorLn options color string = do
  putColor options color string
  when (last string /= '\n') (putStrLn "")

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
