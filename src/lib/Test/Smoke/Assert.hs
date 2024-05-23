module Test.Smoke.Assert (assertResult) where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Data.Default
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Test.Smoke.Filters
import Test.Smoke.Paths
import Test.Smoke.Types

type Asserting = ExceptT SmokeAssertionError IO

assertResult ::
  Path Resolved Dir -> TestPlan -> ExecutionResult -> IO TestResult
assertResult _ TestPlan {planTest = test} ExecutionIgnored =
  pure $ TestIgnored test
assertResult _ TestPlan {planTest = test} (ExecutionFailed exception) =
  pure $ TestErrored test (ExecutionError exception)
assertResult location testPlan@TestPlan {planTest = test} (ExecutionSucceeded actualOutputs) =
  either (TestErrored test . AssertionError) (TestFinished testPlan) <$> runExceptT (processOutputs location testPlan actualOutputs)

processOutputs :: Path Resolved Dir -> TestPlan -> ActualOutputs -> Asserting FinishedTest
processOutputs location (TestPlan _ _ fallbackShell _ _ _ _ expectedStatus expectedStdOuts expectedStdErrs expectedFiles _) (ActualOutputs actualStatus actualStdOut actualStdErr actualFiles) = do
  let statusResult = assertEqual expectedStatus actualStatus
  stdOutResult <- assertAll (defaultIfEmpty expectedStdOuts) actualStdOut
  stdErrResult <- assertAll (defaultIfEmpty expectedStdErrs) actualStdErr
  fileResults <-
    Map.traverseWithKey
      (\relativePath assertions -> assertFile assertions (actualFiles ! (location </> relativePath)))
      expectedFiles
  pure $ FinishedTest statusResult stdOutResult stdErrResult fileResults
  where
    assertEqual :: Eq a => a -> a -> EqualityResult a
    assertEqual expected actual
      | expected == actual = EqualitySuccess
      | otherwise = EqualityFailure (Expected expected) (Actual actual)

    assert :: Assert a -> a -> Asserting (Maybe (AssertionFailure a))
    assert (AssertEquals expected) actual =
      pure $
        if expected == actual
          then Nothing
          else Just $ AssertionFailureDiff (Expected expected) (Actual actual)
    assert (AssertContains expected) actual =
      pure $
        if Text.isInfixOf expected (serializeFixture actual)
          then Nothing
          else Just $ AssertionFailureContains (Expected expected) (Actual actual)
    assert (AssertFiltered fixtureFilter expected) actual = do
      filteredActual <- withExceptT AssertionFilterError $ applyFilters fallbackShell fixtureFilter actual
      assert expected filteredActual
    assert (AssertFileError fileError) actual =
      pure $ Just $ AssertionFailureExpectedFileError fileError (Actual actual)

    assertAll :: Vector (Assert a) -> a -> Asserting (AssertionResult a)
    assertAll expecteds actual = do
      maybeFailures <- sequence <$> Vector.mapM (`assert` actual) expecteds
      pure $ maybe AssertionSuccess (AssertionFailure . collapseAssertionFailures) maybeFailures

    assertFile :: Vector (Assert TestFileContents) -> ActualFile -> Asserting (AssertionResult TestFileContents)
    assertFile assertions (ActualFileContents contents) =
      assertAll assertions contents
    assertFile _ (ActualFileError fileError) =
      pure . AssertionFailure . SingleAssertionFailure $ AssertionFailureActualFileError fileError

    collapseAssertionFailures :: Vector (AssertionFailure a) -> AssertionFailures a
    collapseAssertionFailures failures =
      case Vector.length failures of
        1 -> SingleAssertionFailure (Vector.head failures)
        _ -> MultipleAssertionFailures failures

ifEmpty :: a -> Vector a -> Vector a
ifEmpty x xs
  | Vector.null xs = Vector.singleton x
  | otherwise = xs

defaultIfEmpty :: Default a => Vector a -> Vector a
defaultIfEmpty = ifEmpty def
