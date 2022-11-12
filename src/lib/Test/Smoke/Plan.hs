module Test.Smoke.Plan
  ( planTests,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
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
    forM suites $ \(SuiteWithMetadata suiteName location (Suite thisSuiteWorkingDirectory thisSuiteShellCommandLine thisSuiteCommand tests)) -> do
      let fallbackCommand = thisSuiteCommand <|> specificationCommand
      shell <-
        runExceptT $ mapM shellFromCommandLine thisSuiteShellCommandLine
      case shell of
        Left exception ->
          return $ SuitePlanError suiteName $ SuitePathError exception
        Right fallbackShell -> do
          let fallbackWorkingDirectory = determineWorkingDirectory location thisSuiteWorkingDirectory currentWorkingDirectory
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
  when isInvalid $ throwE NoOutput
  where
    isInvalid = Vector.null (testStdOut test) && Vector.null (testStdErr test) && Map.null (testFiles test)

readTest ::
  Path Resolved Dir ->
  WorkingDirectory ->
  Maybe Shell ->
  Maybe Command ->
  Test ->
  Planning TestPlan
readTest location fallbackWorkingDirectory fallbackShell fallbackCommand test = do
  let workingDirectory = determineWorkingDirectory location (testWorkingDirectory test) fallbackWorkingDirectory
  command <-
    maybe (throwE NoCommand) return (testCommand test <|> fallbackCommand)
  executable <-
    withExceptT PlanningPathError $
      convertCommandToExecutable fallbackShell command
  let args = fromMaybe mempty (testArgs test)
  stdIn <- readStdIn location fallbackShell test
  let status = testStatus test
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

determineWorkingDirectory :: Path Resolved Dir -> Maybe (Path Relative Dir) -> WorkingDirectory -> WorkingDirectory
determineWorkingDirectory location workingDirectory fallbackWorkingDirectory =
  maybe fallbackWorkingDirectory (WorkingDirectory . (location </>)) workingDirectory

readStdIn :: Path Resolved Dir -> Maybe Shell -> Test -> Planning StdIn
readStdIn location fallbackShell test =
  fromMaybe (StdIn Text.empty) <$> mapM (readTestInput location fallbackShell) (testStdIn test)

readStdOut :: Path Resolved Dir -> Test -> Planning (Vector (Assert StdOut))
readStdOut location test = mapM (readTestOutput location) (testStdOut test)

readStdErr :: Path Resolved Dir -> Test -> Planning (Vector (Assert StdErr))
readStdErr location test = mapM (readTestOutput location) (testStdErr test)

readFiles :: Path Resolved Dir -> Test -> Planning (Map (Path Relative File) (Vector (Assert TestFileContents)))
readFiles location test = mapM (mapM (readTestOutput location)) (testFiles test)

readTestInput :: (ToFixture a, FromFixture a) => Path Resolved Dir -> Maybe Shell -> TestInput a -> Planning a
readTestInput _ _ (TestInputInline contents) =
  pure contents
readTestInput location _ (TestInputFromFile path) =
  withExceptT PlanningFixtureFileError $ readPath location path
readTestInput location fallbackShell (TestInputFiltered inner fixtureFilter) = do
  value <- readTestInput location fallbackShell inner
  withExceptT PlanningFilterError $ applyFilters fallbackShell fixtureFilter value

readTestOutput :: Path Resolved Dir -> TestOutput a -> Planning (Assert a)
readTestOutput _ (TestOutputInline assertion) =
  pure assertion
readTestOutput location (TestOutputFromFile constructor contents) =
  liftIO $ either AssertFileError constructor <$> runExceptT (readPath location contents)

readPath :: ToFixture a => Path Resolved Dir -> Path Relative File -> ExceptT SmokeFileError IO a
readPath location path =
  deserializeFixture
    <$> withExceptT
      (handleMissingFileError path)
      (ExceptT $ tryIOError $ readFromPath (location </> path))

handleMissingFileError :: Path Relative File -> IOError -> SmokeFileError
handleMissingFileError path e =
  if isDoesNotExistError e
    then MissingFile path
    else CouldNotReadFile path e
