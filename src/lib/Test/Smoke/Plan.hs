module Test.Smoke.Plan
  ( planTests,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.IO.Error (isDoesNotExistError, tryIOError)
import Test.Smoke.Executable
import Test.Smoke.Filters
import Test.Smoke.Paths
import Test.Smoke.Types

type Planning = ExceptT SmokePlanningError IO

planTests :: TestSpecification -> IO Plan
planTests (TestSpecification specificationCommand suites) = do
  currentWorkingDirectory <- WorkingDirectory <$> getCurrentWorkingDirectory
  suitePlans <-
    forM suites $ \(suiteName, suite) ->
      case suite of
        Left exception ->
          return $ SuitePlanError suiteName $ SuiteDiscoveryError exception
        Right (Suite location thisSuiteWorkingDirectory thisSuiteShellCommandLine thisSuiteCommand tests) -> do
          let fallbackCommand = thisSuiteCommand <|> specificationCommand
          shell <-
            runExceptT $ mapM shellFromCommandLine thisSuiteShellCommandLine
          case shell of
            Left exception ->
              return $ SuitePlanError suiteName $ SuitePathError exception
            Right fallbackShell -> do
              let fallbackWorkingDirectory =
                    fromMaybe currentWorkingDirectory thisSuiteWorkingDirectory
              testPlans <-
                forM tests $ \test ->
                  either (TestPlanError test) TestPlanSuccess
                    <$> runExceptT
                      ( do
                          validateTest fallbackCommand test
                          readTest
                            location
                            fallbackWorkingDirectory
                            fallbackShell
                            fallbackCommand
                            test
                      )
              return $ SuitePlan suiteName location testPlans
  return $ Plan suitePlans

validateTest :: Maybe Command -> Test -> Planning ()
validateTest fallbackCommand test = do
  when (isNothing (testCommand test <|> fallbackCommand)) $ throwE NoCommand
  when
    ( isEmptyFixtures (testStdOut test)
        && isEmptyFixtures (testStdErr test)
        && isEmptyFiles (testFiles test)
    )
    $ throwE NoOutput
  where
    isEmptyFixtures (Fixtures fixtures) = Vector.null fixtures
    isEmptyFiles = Map.null

readTest ::
  ResolvedPath Dir ->
  WorkingDirectory ->
  Maybe Shell ->
  Maybe Command ->
  Test ->
  Planning TestPlan
readTest location fallbackWorkingDirectory fallbackShell fallbackCommand test = do
  let workingDirectory =
        fromMaybe fallbackWorkingDirectory (testWorkingDirectory test)
  command <-
    maybe (throwE NoCommand) return (testCommand test <|> fallbackCommand)
  executable <-
    withExceptT PlanningPathError $
      convertCommandToExecutable fallbackShell command
  let args = fromMaybe mempty (testArgs test)
  stdIn <- readStdIn location fallbackShell test
  status <- readStatus location test
  stdOut <- readStdOut location test
  stdErr <- readStdErr location test
  files <- readFiles location test
  let revert = Vector.map (location </>) (testRevert test)
  return $
    TestPlan
      { planTest = test,
        planWorkingDirectory = workingDirectory,
        planShell = fallbackShell,
        planExecutable = executable,
        planArgs = args,
        planStdIn = stdIn,
        planStatus = status,
        planStdOut = stdOut,
        planStdErr = stdErr,
        planFiles = files,
        planRevert = revert
      }

readStdIn :: ResolvedPath Dir -> Maybe Shell -> Test -> Planning StdIn
readStdIn location fallbackShell test = do
  unfilteredStdIn <-
    fromMaybe (Unfiltered (StdIn Text.empty))
      <$> sequence (readFixture location <$> testStdIn test)
  withExceptT PlanningFilterError $ applyFilters fallbackShell unfilteredStdIn

readStatus :: ResolvedPath Dir -> Test -> Planning (Assert Status)
readStatus location test = AssertEqual . unfiltered <$> readFixture location (testStatus test)

readStdOut :: ResolvedPath Dir -> Test -> Planning (Vector (Assert StdOut))
readStdOut location test = Vector.map (AssertEqual . unfiltered) <$> readFixtures location (testStdOut test)

readStdErr :: ResolvedPath Dir -> Test -> Planning (Vector (Assert StdErr))
readStdErr location test = Vector.map (AssertEqual . unfiltered) <$> readFixtures location (testStdErr test)

readFiles :: ResolvedPath Dir -> Test -> Planning (Map (RelativePath File) (Vector (Assert TestFileContents)))
readFiles location test = mapM (fmap (Vector.map (AssertEqual . unfiltered)) . readFixtures location) (testFiles test)

readFixture ::
  FixtureType a => ResolvedPath Dir -> Fixture a -> Planning (Filtered a)
readFixture _ (Fixture (Inline contents) maybeFilter) =
  return $ includeFilter maybeFilter contents
readFixture location (Fixture (FileLocation path) maybeFilter) =
  includeFilter maybeFilter . deserializeFixture
    <$> withExceptT
      (handleMissingFileError path)
      (ExceptT $ tryIOError $ readFromPath (location </> path))

readFixtures ::
  FixtureType a =>
  ResolvedPath Dir ->
  Fixtures a ->
  Planning (Vector (Filtered a))
readFixtures location (Fixtures fixtures) = mapM (readFixture location) fixtures

includeFilter :: Maybe Command -> a -> Filtered a
includeFilter maybeFilter contents =
  maybe (Unfiltered contents) (Filtered contents) maybeFilter

handleMissingFileError :: RelativePath File -> IOError -> SmokePlanningError
handleMissingFileError path e =
  if isDoesNotExistError e
    then NonExistentFixture path
    else CouldNotReadFixture path e
