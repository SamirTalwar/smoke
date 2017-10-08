module Main where

import Options
import Test.Smoke

main :: IO ()
main = do
  options <- parseOptions
  tests <- discoverTests options
  results <- runTests tests
  printResults results
  printSummary results

printResults :: TestResults -> IO ()
printResults = mapM_ print

printSummary :: TestResults -> IO ()
printSummary _ = return ()
