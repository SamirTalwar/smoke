module Test.Smoke.Plan
  ( planTests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Directory (doesFileExist, findExecutable)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Test.Smoke.Errors
import Test.Smoke.Files
import Test.Smoke.Filters
import Test.Smoke.Types

type Planning = ExceptT SmokePlanningError IO

type ExpectedOutputs = (Status, Vector StdOut, Vector StdErr)

planTests :: TestSpecification -> IO Plan
planTests (TestSpecification specificationCommand suites) = do
  suitePlans <-
    forM suites $ \(suiteName, suite) ->
      case suite of
        Left errorMessage -> return (suiteName, Left errorMessage)
        Right (Suite thisSuiteCommand tests) -> do
          let defaultCommand = thisSuiteCommand <|> specificationCommand
          testPlans <-
            forM tests $ \test ->
              runExceptT $
              withExceptT (TestPlanError test) $ do
                validateTest defaultCommand test
                readTest defaultCommand test
          return (suiteName, Right testPlans)
  return $ Plan suitePlans

validateTest :: Maybe Command -> Test -> Planning ()
validateTest defaultCommand test = do
  when (isNothing (testCommand test <|> defaultCommand)) $ throwE NoCommand
  when (isNothing (testArgs test) && isNothing (testStdIn test)) $
    throwE NoInput
  when (isEmpty (testStdOut test) && isEmpty (testStdErr test)) $
    throwE NoOutput
  where
    isEmpty (Fixtures fixtures) = Vector.null fixtures

readTest :: Maybe Command -> Test -> Planning TestPlan
readTest defaultCommand test = do
  (executable, args) <-
    splitCommand (testCommand test <|> defaultCommand) (testArgs test)
  let executableName = show $ unExecutable executable
  executableExists <- liftIO (doesFileExist executableName)
  unless executableExists $
    onNothingThrow_ (NonExistentCommand executable) =<<
    liftIO (findExecutable executableName)
  unfilteredStdIn <-
    fromMaybe (Unfiltered (StdIn Text.empty)) <$>
    sequence (readFixture <$> testStdIn test)
  filteredStdIn <-
    withExceptT PlanningFilterError $ applyFilters unfilteredStdIn
  (status, stdOut, stdErr) <- readExpectedOutputs test
  return $
    TestPlan
      { planTest = test
      , planExecutable = executable
      , planArgs = args
      , planStdIn = filteredStdIn
      , planStatus = status
      , planStdOut = stdOut
      , planStdErr = stdErr
      }

splitCommand :: Maybe Command -> Maybe Args -> Planning (Executable, Args)
splitCommand maybeCommand maybeArgs = do
  (executableName:commandArgs) <-
    onNothingThrow NoCommand (unCommand <$> maybeCommand)
  let args = commandArgs ++ maybe [] unArgs maybeArgs
  return (Executable (makePath executableName), Args args)

readExpectedOutputs :: Test -> Planning ExpectedOutputs
readExpectedOutputs test = do
  expectedStatus <- unfiltered <$> readFixture (testStatus test)
  expectedStdOuts <- Vector.map unfiltered <$> readFixtures (testStdOut test)
  expectedStdErrs <- Vector.map unfiltered <$> readFixtures (testStdErr test)
  return (expectedStatus, expectedStdOuts, expectedStdErrs)

readFixture :: FixtureType a => Fixture a -> Planning (Filtered a)
readFixture (Fixture (Inline contents) maybeFilter) =
  return $ includeFilter maybeFilter contents
readFixture (Fixture (FileLocation path) maybeFilter) =
  includeFilter maybeFilter . deserializeFixture <$>
  withExceptT
    (handleMissingFileError path)
    (ExceptT $ tryIOError $ readFromPath path)

readFixtures :: FixtureType a => Fixtures a -> Planning (Vector (Filtered a))
readFixtures (Fixtures fixtures) = mapM readFixture fixtures

includeFilter :: Maybe FixtureFilter -> a -> Filtered a
includeFilter maybeFilter contents =
  maybe (Unfiltered contents) (Filtered contents) maybeFilter

handleMissingFileError :: Path -> IOError -> SmokePlanningError
handleMissingFileError path e =
  if isDoesNotExistError e
    then NonExistentFixture path
    else CouldNotReadFixture path (show e)
