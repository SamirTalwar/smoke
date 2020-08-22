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
  ResolvedPath Dir ->
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

determineWorkingDirectory :: ResolvedPath Dir -> Maybe (RelativePath Dir) -> WorkingDirectory -> WorkingDirectory
determineWorkingDirectory location workingDirectory fallbackWorkingDirectory =
  maybe fallbackWorkingDirectory (WorkingDirectory . (location </>)) workingDirectory

readStdIn :: ResolvedPath Dir -> Maybe Shell -> Test -> Planning StdIn
readStdIn location fallbackShell test =
  fromMaybe (StdIn Text.empty) <$> sequence (readTestInput location fallbackShell <$> testStdIn test)

readStatus :: ResolvedPath Dir -> Test -> Planning (Assert Status)
readStatus location test = readTestOutput location (testStatus test)

readStdOut :: ResolvedPath Dir -> Test -> Planning (Vector (Assert StdOut))
readStdOut location test = mapM (readTestOutput location) (testStdOut test)

readStdErr :: ResolvedPath Dir -> Test -> Planning (Vector (Assert StdErr))
readStdErr location test = mapM (readTestOutput location) (testStdErr test)

readFiles :: ResolvedPath Dir -> Test -> Planning (Map (RelativePath File) (Vector (Assert TestFileContents)))
readFiles location test = mapM (mapM (readTestOutput location)) (testFiles test)

readTestInput :: FixtureType a => ResolvedPath Dir -> Maybe Shell -> TestInput a -> Planning a
readTestInput location fallbackShell (TestInput maybeFilter contents) = do
  value <- readContents location contents
  maybe (return value) (filtered value) maybeFilter
  where
    filtered :: FixtureType a => a -> Filter -> Planning a
    filtered value fixtureFilter = withExceptT PlanningFilterError $ applyFilters fallbackShell fixtureFilter value

readTestOutput :: FixtureType a => ResolvedPath Dir -> TestOutput a -> Planning (Assert a)
readTestOutput location (TestOutput constructor _ contents) =
  constructor <$> readContents location contents

readContents :: FixtureType a => ResolvedPath Dir -> Contents a -> Planning a
readContents _ (Inline value) =
  return value
readContents location (FileLocation path) =
  deserializeFixture
    <$> withExceptT
      (handleMissingFileError path)
      (ExceptT $ tryIOError $ readFromPath (location </> path))

handleMissingFileError :: RelativePath File -> IOError -> SmokePlanningError
handleMissingFileError path e =
  if isDoesNotExistError e
    then NonExistentFixture path
    else CouldNotReadFixture path e
