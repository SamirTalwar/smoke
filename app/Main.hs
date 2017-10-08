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
  printResults results
  printSummary results
  exitAccordingTo results

printResults :: TestResults -> IO ()
printResults = mapM_ printResult

printResult :: TestResult -> IO ()
printResult (TestSuccess test) = do
  putStrLn (testName test)
  putGreenLn "  succeeded"
printResult (TestFailure test actualStatus actualStdOut actualStdErr stdIn expectedStatus expectedStdOuts expectedStdErrs) = do
  putStrLn (testName test)
  when (actualStatus /= expectedStatus) $ do
    putRed "  actual status:    "
    putRedLn (show actualStatus)
    putRed "  expected status:  "
    putRedLn (show expectedStatus)
  forM_ stdIn $ \input -> do
    putRed "  input:            "
    putRedLn input
  when (actualStdOut `notElem` expectedStdOuts) $ do
    putRed "  actual output:    "
    putRedLn actualStdOut
    putRed "  expected output:  "
    putRedLn (head expectedStdOuts)
    forM_ (tail expectedStdOuts) $ \output -> do
      putRed "               or:  "
      putRedLn output
  when (actualStdErr `notElem` expectedStdErrs) $ do
    putRed "  actual error:     "
    putRedLn actualStdErr
    putRed "  expected error:   "
    putRedLn (head expectedStdErrs)
    forM_ (tail expectedStdErrs) $ \output -> do
      putRed "              or:   "
      putRedLn output
printResult (TestError test CouldNotFindExecutable) = do
  putStrLn (testName test)
  putRedLn "  could not find the executable"

printSummary :: TestResults -> IO ()
printSummary results = do
  putStrLn ""
  let testCount = length results
  let failureCount = length failures
  case failureCount of
    0 -> putGreenLn (show testCount ++ " tests, 0 failures")
    1 -> putRedLn (show testCount ++ " tests, 1 failure")
    n -> putRedLn (show testCount ++ " tests, " ++ show n ++ " failures")
  where
    failures = filter isFailure results

putGreen :: String -> IO ()
putGreen = putColor Green

putGreenLn :: String -> IO ()
putGreenLn = putColorLn Green

putRed :: String -> IO ()
putRed = putColor Red

putRedLn :: String -> IO ()
putRedLn = putColorLn Red

putColor :: Color -> String -> IO ()
putColor color string = do
  setSGR [SetColor Foreground Dull color]
  putStr string
  setSGR [Reset]

putColorLn :: Color -> String -> IO ()
putColorLn color string = do
  putColor color string
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
