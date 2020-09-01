module Test.Smoke.Assert (assertResult) where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Data.Default
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Filters
import Test.Smoke.Paths
import Test.Smoke.Types

type Asserting = ExceptT SmokeAssertionError IO

assertResult ::
  ResolvedPath Dir -> TestPlan -> ExecutionResult -> IO TestOutcome
assertResult _ _ ExecutionIgnored =
  return TestIgnored
assertResult _ _ (ExecutionFailed exception) =
  return $ TestError (ExecutionError exception)
assertResult location testPlan (ExecutionSucceeded actualOutputs) =
  either (TestError . AssertionError) id <$> runExceptT (processOutputs location testPlan actualOutputs)

processOutputs :: ResolvedPath Dir -> TestPlan -> ActualOutputs -> Asserting TestOutcome
processOutputs location testPlan@(TestPlan _ _ fallbackShell _ _ _ expectedStatus expectedStdOuts expectedStdErrs expectedFiles _) (ActualOutputs actualStatus actualStdOut actualStdErr actualFiles) = do
  statusResult <- assertSingle expectedStatus actualStatus
  stdOutResult <- assertAll (defaultIfEmpty expectedStdOuts) actualStdOut
  stdErrResult <- assertAll (defaultIfEmpty expectedStdErrs) actualStdErr
  fileResults <-
    Map.traverseWithKey
      (\relativePath contents -> assertAll contents (actualFiles ! (location </> relativePath)))
      expectedFiles
  return $
    if isPartSuccess statusResult
      && isPartSuccess stdOutResult
      && isPartSuccess stdErrResult
      && all isPartSuccess (Map.elems fileResults)
      then TestSuccess
      else
        TestFailure
          testPlan
          statusResult
          stdOutResult
          stdErrResult
          fileResults
  where
    assert :: FixtureType a => Assert a -> a -> Asserting (Maybe (AssertionFailure a))
    assert (AssertEquals expected) actual =
      return $
        if expected == actual
          then Nothing
          else Just $ AssertionFailureDiff expected actual
    assert (AssertContains expected) actual =
      return $
        if Text.isInfixOf (serializeFixture expected) (serializeFixture actual)
          then Nothing
          else Just $ AssertionFailureContains expected actual
    assert (AssertFiltered fixtureFilter expected) actual = do
      filteredActual <- withExceptT AssertionFilterError $ applyFilters fallbackShell fixtureFilter actual
      assert expected filteredActual

    assertSingle :: FixtureType a => Assert a -> a -> Asserting (PartResult a)
    assertSingle expected actual =
      maybe PartSuccess (PartFailure . SingleAssertionFailure) <$> assert expected actual

    assertAll :: FixtureType a => Vector (Assert a) -> a -> Asserting (PartResult a)
    assertAll expecteds actual = do
      maybeFailures <- sequence <$> Vector.mapM (`assert` actual) expecteds
      return $ maybe PartSuccess (PartFailure . collapseAssertionFailures) maybeFailures

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
