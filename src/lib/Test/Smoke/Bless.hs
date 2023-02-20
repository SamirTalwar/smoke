{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Smoke.Bless
  ( blessResult,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad (foldM)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types

blessResult :: Path Resolved Dir -> TestResult -> IO TestResult
blessResult location (TestFinished testPlan finishedTest) = blessFinishedTest location testPlan finishedTest
blessResult _ result = pure result

blessFinishedTest :: Path Resolved Dir -> TestPlan -> FinishedTest -> IO TestResult
blessFinishedTest _ TestPlan {planTest = test} (FinishedTest (EqualityFailure _ (Actual (Status actualStatus))) _ _ _) =
  failed test $ CouldNotBlessInlineFixture "status" (Text.pack (show actualStatus))
blessFinishedTest location testPlan@(TestPlan {planTest = test}) result@(FinishedTest _ stdOut stdErr files) =
  do
    serializedStdOut <- serialize (testStdOut test) stdOut
    serializedStdErr <- serialize (testStdErr test) stdErr
    serializedFiles <- Map.traverseWithKey (\path fileResult -> serialize (testFiles test ! path) fileResult) files
    TestFinished testPlan
      <$> foldM
        (\r f -> f r)
        result
        ( writeFixture serializedStdOut (\a r -> r {finishedTestStdOut = a})
            : writeFixture serializedStdErr (\a r -> r {finishedTestStdErr = a})
            : map (\(path, serializedFile) -> writeFixture serializedFile (\a r -> r {finishedTestFiles = Map.insert path a (finishedTestFiles r)})) (Map.assocs serializedFiles)
        )
    `catch` (\(e :: SmokeBlessError) -> failed test e)
    `catch` (failed test . BlessIOException)
  where
    writeFixture :: Maybe (Path Relative File, Text) -> (AssertionResult a -> FinishedTest -> FinishedTest) -> FinishedTest -> IO FinishedTest
    writeFixture Nothing _ before =
      return before
    writeFixture (Just (path, text)) makeAfter before = do
      createParent (location </> path)
      writeToPath (location </> path) text
      return $ makeAfter AssertionSuccess before

serialize :: forall a. FromFixture a => Vector (TestOutput a) -> AssertionResult a -> IO (Maybe (Path Relative File, Text))
serialize _ AssertionSuccess =
  return Nothing
serialize outputs (AssertionFailure result) =
  case Vector.length outputs of
    1 ->
      serializeFailure (Vector.head outputs) result
    0 ->
      throwIO $ CouldNotBlessAMissingValue (fixtureName @a)
    _ ->
      throwIO $ CouldNotBlessWithMultipleValues (fixtureName @a)

serializeFailure :: forall a. FromFixture a => TestOutput a -> AssertionFailures a -> IO (Maybe (Path Relative File, Text))
serializeFailure (TestOutputFromFile _ path) (SingleAssertionFailure (AssertionFailureDiff _ (Actual actual))) =
  return $ Just (path, serializeFixture actual)
serializeFailure _ (SingleAssertionFailure (AssertionFailureDiff _ (Actual actual))) =
  throwIO $ CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture actual)
serializeFailure _ (SingleAssertionFailure (AssertionFailureContains _ (Actual actual))) =
  throwIO $ CouldNotBlessContainsAssertion (fixtureName @a) (serializeFixture actual)
serializeFailure (TestOutputFromFile _ path) (SingleAssertionFailure (AssertionFailureExpectedFileError _ (Actual actual))) =
  return $ Just (path, serializeFixture actual)
serializeFailure _ (SingleAssertionFailure (AssertionFailureExpectedFileError _ (Actual actual))) =
  throwIO $ CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture actual)
serializeFailure _ (SingleAssertionFailure (AssertionFailureActualFileError _)) =
  return Nothing
serializeFailure _ (MultipleAssertionFailures _) =
  throwIO $ CouldNotBlessWithMultipleValues (fixtureName @a)

failed :: Applicative f => Test -> SmokeBlessError -> f TestResult
failed test = pure . TestErrored test . BlessError
