module Main where

import Control.Exception (catch)
import Control.Monad.Trans.Reader (runReaderT)
import System.Exit
import Test.Smoke
import Test.Smoke.App.OptionTypes
import Test.Smoke.App.Options
import Test.Smoke.App.Print
import Test.Smoke.App.PrintErrors
import Test.Smoke.App.PrintResults

main :: IO ()
main = do
  options <- parseOptions
  run options `catch` \discoveryError -> do
    flip runReaderT options $ printDiscoveryError putError discoveryError
    exitWith (ExitFailure 2)

run :: AppOptions -> IO ()
run options = do
  tests <- discoverTests (optionsExecution options)
  plan <- planTests tests
  results <- runTests plan
  modifiedResults <-
    case optionsMode options of
      Check -> return results
      Bless -> blessResults results
  summary <- flip runReaderT options $ outputResults modifiedResults
  if summaryFailures summary == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
