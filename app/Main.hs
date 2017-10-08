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
    putRedLn options (indented input)
  when (actualStdOut `notElem` expectedStdOuts) $ do
    putRed options "  actual output:    "
    putRedLn options (indented actualStdOut)
    putRed options "  expected output:  "
    putRedLn options (indented (head expectedStdOuts))
    forM_ (tail expectedStdOuts) $ \output -> do
      putRed options "               or:  "
      putRedLn options (indented output)
  when (actualStdErr `notElem` expectedStdErrs) $ do
    putRed options "  actual error:     "
    putRedLn options (indented actualStdErr)
    putRed options "  expected error:   "
    putRedLn options (indented (head expectedStdErrs))
    forM_ (tail expectedStdErrs) $ \output -> do
      putRed options "              or:   "
      putRedLn options (indented output)
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

indentationPrefix :: String
indentationPrefix = replicate 20 ' '

indented :: String -> String
indented string = unlines $ first : map (indentationPrefix ++) rest
  where
    (first:rest) = lines string

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
