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
  currentWorkingDirectory <- WorkingDirectory <$> getCurrentWorkingDirectory
  suitePlans <-
    forM suites $ \(suiteName, suite) ->
      case suite of
        Left errorMessage -> return $ SuitePlanError suiteName errorMessage
        Right (Suite location thisSuiteWorkingDirectory thisSuiteCommand tests) -> do
          let defaultCommand = thisSuiteCommand <|> specificationCommand
          let defaultWorkingDirectory =
                fromMaybe currentWorkingDirectory thisSuiteWorkingDirectory
          testPlans <-
            forM tests $ \test ->
              runExceptT $
              withExceptT (TestPlanError test) $ do
                validateTest defaultCommand test
                readTest location defaultWorkingDirectory defaultCommand test
          return $ SuitePlan suiteName location testPlans
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

readTest ::
     Path -> WorkingDirectory -> Maybe Command -> Test -> Planning TestPlan
readTest location defaultWorkingDirectory defaultCommand test = do
  workingDirectory <-
    resolveWorkingDirectory $
    fromMaybe defaultWorkingDirectory (testWorkingDirectory test)
  (executable, args) <-
    splitCommand (testCommand test <|> defaultCommand) (testArgs test)
  let executableName = show $ unExecutable executable
  executableExists <- liftIO (doesFileExist executableName)
  unless executableExists $
    onNothingThrow_ (NonExistentCommand executable) =<<
    liftIO (findExecutable executableName)
  unfilteredStdIn <-
    fromMaybe (Unfiltered (StdIn Text.empty)) <$>
    sequence (readFixture location <$> testStdIn test)
  filteredStdIn <-
    withExceptT PlanningFilterError $ applyFilters unfilteredStdIn
  (status, stdOut, stdErr) <- readExpectedOutputs location test
  return $
    TestPlan
      { planTest = test
      , planWorkingDirectory = workingDirectory
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

readExpectedOutputs :: Path -> Test -> Planning ExpectedOutputs
readExpectedOutputs location test = do
  expectedStatus <- unfiltered <$> readFixture location (testStatus test)
  expectedStdOuts <-
    Vector.map unfiltered <$> readFixtures location (testStdOut test)
  expectedStdErrs <-
    Vector.map unfiltered <$> readFixtures location (testStdErr test)
  return (expectedStatus, expectedStdOuts, expectedStdErrs)

readFixture :: FixtureType a => Path -> Fixture a -> Planning (Filtered a)
readFixture _ (Fixture (Inline contents) maybeFilter) =
  return $ includeFilter maybeFilter contents
readFixture location (Fixture (FileLocation path) maybeFilter) =
  includeFilter maybeFilter . deserializeFixture <$>
  withExceptT
    (handleMissingFileError path)
    (ExceptT $ tryIOError $ readFromPath (location </> path))

readFixtures ::
     FixtureType a => Path -> Fixtures a -> Planning (Vector (Filtered a))
readFixtures location (Fixtures fixtures) = mapM (readFixture location) fixtures

includeFilter :: Maybe FixtureFilter -> a -> Filtered a
includeFilter maybeFilter contents =
  maybe (Unfiltered contents) (Filtered contents) maybeFilter

handleMissingFileError :: Path -> IOError -> SmokePlanningError
handleMissingFileError path e =
  if isDoesNotExistError e
    then NonExistentFixture path
    else CouldNotReadFixture path (show e)

resolveWorkingDirectory :: WorkingDirectory -> Planning WorkingDirectory
resolveWorkingDirectory workingDirectory =
  WorkingDirectory <$>
  (liftIO . resolvePath $ unWorkingDirectory workingDirectory)
