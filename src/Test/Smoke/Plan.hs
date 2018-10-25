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
import Test.Smoke.Types

type Planning = ExceptT TestPlanErrorMessage IO

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
  (executable@(Executable executableName), args) <-
    splitCommand (testCommand test <|> defaultCommand) (testArgs test)
  executableExists <- liftIO (doesFileExist executableName)
  unless executableExists $
    onNothingThrow_ (NonExistentCommand executable) =<<
    liftIO (findExecutable executableName)
  stdIn <-
    fromMaybe (StdIn Text.empty) <$> sequence (readFixture <$> testStdIn test)
  (status, stdOut, stdErr) <- readExpectedOutputs test
  return $
    TestPlan
      { planTest = test
      , planExecutable = executable
      , planArgs = args
      , planStdIn = stdIn
      , planStatus = status
      , planStdOut = stdOut
      , planStdErr = stdErr
      }

splitCommand :: Maybe Command -> Maybe Args -> Planning (Executable, Args)
splitCommand maybeCommand maybeArgs = do
  (executableName:commandArgs) <-
    onNothingThrow NoCommand (unCommand <$> maybeCommand)
  let args = commandArgs ++ maybe [] unArgs maybeArgs
  return (Executable executableName, Args args)

readExpectedOutputs :: Test -> Planning ExpectedOutputs
readExpectedOutputs test = do
  expectedStatus <- readFixture (testStatus test)
  expectedStdOuts <- readFixtures (StdOut Text.empty) (testStdOut test)
  expectedStdErrs <- readFixtures (StdErr Text.empty) (testStdErr test)
  return (expectedStatus, expectedStdOuts, expectedStdErrs)

readFixture :: FixtureContents a => Fixture a -> Planning a
readFixture (InlineFixture contents) = return contents
readFixture (FileFixture path) =
  deserializeFixture <$>
  withExceptT
    (handleMissingFileError path)
    (ExceptT $ tryIOError $ readFromPath path)

readFixtures :: FixtureContents a => a -> Fixtures a -> Planning (Vector a)
readFixtures defaultValue (Fixtures fixtures) =
  ifEmpty defaultValue <$> mapM readFixture fixtures
  where
    ifEmpty :: a -> Vector a -> Vector a
    ifEmpty value xs
      | Vector.null xs = Vector.singleton value
      | otherwise = xs

handleMissingFileError :: Path -> IOError -> TestPlanErrorMessage
handleMissingFileError path e =
  if isDoesNotExistError e
    then NonExistentFixture path
    else CouldNotReadFixture path (show e)
