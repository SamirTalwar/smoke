{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.App.PrintErrors
  ( printError
  , printDiscoveryError
  , printExecutableError
  , printSuiteError
  , printTestError
  ) where

import Control.Exception (displayException)
import Data.String (fromString)
import Data.Text (Text)
import Test.Smoke
import Test.Smoke.App.Print

printSuiteError :: SuiteError -> Output ()
printSuiteError (SuiteDiscoveryError discoveryError) =
  printDiscoveryError printError discoveryError
printSuiteError (SuiteExecutableError executableError) =
  printExecutableError executableError

printTestError :: SmokeError -> Output ()
printTestError (DiscoveryError discoveryError) =
  printDiscoveryError printError discoveryError
printTestError (PlanningError NoCommand) = printError "There is no command."
printTestError (PlanningError NoInput) =
  printError "There are no args or STDIN values in the specification."
printTestError (PlanningError NoOutput) =
  printError
    "There are no STDOUT or STDERR values, or files, in the specification."
printTestError (PlanningError (NonExistentFixture path)) =
  printError $ "The fixture " <> showPath path <> " does not exist."
printTestError (PlanningError (CouldNotReadFixture path exception)) =
  printErrorWithException exception $
  "The fixture " <> showPath path <> " could not be read."
printTestError (PlanningError (PlanningFilterError filterError)) =
  printFilterError filterError
printTestError (PlanningError (PlanningExecutableError executableError)) =
  printExecutableError executableError
printTestError (ExecutionError (NonExistentWorkingDirectory (WorkingDirectory path))) =
  printError $ "The working directory " <> showPath path <> " does not exist."
printTestError (ExecutionError (CouldNotExecuteCommand executable exception)) =
  printErrorWithException exception $
  showExecutable executable <> " could not be executed."
printTestError (ExecutionError (CouldNotReadFile path exception)) =
  printErrorWithException exception $
  "The output file " <> showPath path <> " does not exist."
printTestError (ExecutionError (CouldNotStoreDirectory path exception)) =
  printErrorWithException exception $
  "The directory " <> showPath path <> " could not be stored."
printTestError (ExecutionError (CouldNotRevertDirectory path exception)) =
  printErrorWithException exception $
  "The directory " <> showPath path <> " could not be reverted."
printTestError (ExecutionError (ExecutionFilterError filterError)) =
  printFilterError filterError
printTestError (BlessError (CouldNotBlessInlineFixture (FixtureName fixtureName') propertyValue)) =
  printError $
  "The fixture " <> quoteString fixtureName' <>
  " is embedded in the test specification, so the result cannot be blessed.\nAttempted to write:\n" <>
  indentedAll messageIndentation propertyValue
printTestError (BlessError (CouldNotBlessAMissingValue (FixtureName fixtureName'))) =
  printError $
  "There are no expected " <> quoteString fixtureName' <>
  " values, so the result cannot be blessed.\n"
printTestError (BlessError (CouldNotBlessWithMultipleValues (FixtureName fixtureName'))) =
  printError $
  "There are multiple expected " <> quoteString fixtureName' <>
  " values, so the result cannot be blessed.\n"
printTestError (BlessError (BlessIOException exception)) =
  printErrorWithException exception "Blessing failed."

printDiscoveryError :: (Text -> Output ()) -> SmokeDiscoveryError -> Output ()
printDiscoveryError printErrorMessage = printErrorMessage . printDiscoveryError'
  where
    printDiscoveryError' :: SmokeDiscoveryError -> Text
    printDiscoveryError' (NoSuchLocation path) =
      "There is no such location " <> quoteString path <> "."
    printDiscoveryError' (NoSuchTest path (TestName selectedTestName)) =
      "There is no such test " <> quoteString selectedTestName <> " in " <>
      showPath path <>
      "."
    printDiscoveryError' (CannotSelectTestInDirectory path (TestName selectedTestName)) =
      "The test " <> quoteString selectedTestName <>
      " cannot be selected from the directory " <>
      showPath path <>
      ".\n" <>
      "Tests must be selected from a single specification file."
    printDiscoveryError' (InvalidSpecification path message) =
      "The test specification " <> showPath path <> " is invalid:\n" <>
      indentedAll messageIndentation (fromString message)

printFilterError :: SmokeFilterError -> Output ()
printFilterError MissingFilterScript =
  printError "The filter script is missing."
printFilterError (CouldNotExecuteFilter executable exception) =
  printErrorWithException exception $
  showExecutable executable <> " could not be executed."
printFilterError (ExecutionFailed executable (Status status) (StdOut stdOut) (StdErr stdErr)) =
  printError $
  showExecutable executable <> " failed with an exit status of " <>
  showInt status <>
  "." <>
  "\nSTDOUT:\n" <>
  indentedAll messageIndentation stdOut <>
  "\nSTDERR:\n" <>
  indentedAll messageIndentation stdErr
printFilterError (FilterExecutableError executableError) =
  printExecutableError executableError

printExecutableError :: SmokeExecutableError -> Output ()
printExecutableError (CouldNotFindExecutable path) =
  printError $ "The executable " <> showPath path <> " could not be found."
printExecutableError (FileIsNotExecutable path) =
  printError $ "The file at " <> showPath path <> " is not executable."

printError :: Text -> Output ()
printError = putRedLn . indentedAll messageIndentation

printErrorWithException :: IOError -> Text -> Output ()
printErrorWithException exception =
  putRedLn .
  indentedAll messageIndentation .
  (<> "\n" <> fromString (displayException exception))
