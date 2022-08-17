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
blessResult _ (TestResult TestPlan {planTest = test} (EqualityFailure _ (Actual (Status actualStatus))) _ _ _) =
  failed test $ CouldNotBlessInlineFixture "status" (Text.pack (show actualStatus))
blessResult location result@(TestResult TestPlan {planTest = test} _ stdOut stdErr files) =
  do
    serializedStdOut <- serialize (testStdOut test) stdOut
    serializedStdErr <- serialize (testStdErr test) stdErr
    serializedFiles <- Map.traverseWithKey (\path fileResult -> serialize (testFiles test ! path) fileResult) files
    foldM
      (\r f -> f r)
      result
      ( writeFixture serializedStdOut (\a r -> r {resultStdOut = a}) :
        writeFixture serializedStdErr (\a r -> r {resultStdErr = a}) :
        map (\(path, serializedFile) -> writeFixture serializedFile (\a r -> r {resultFiles = Map.insert path a (resultFiles r)})) (Map.assocs serializedFiles)
      )
    `catch` (\(e :: SmokeBlessError) -> failed test e)
    `catch` (failed test . BlessIOException)
  where
    writeFixture :: Maybe (Path Relative File, Text) -> (AssertionResult a -> TestResult -> TestResult) -> TestResult -> IO TestResult
    writeFixture Nothing _ before =
      return before
    writeFixture (Just (path, text)) makeAfter before = do
      createParent (location </> path)
      writeToPath (location </> path) text
      return $ makeAfter AssertionSuccess before
blessResult _ result = return result

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
serializeFailure (TestOutput _ (FileLocation path)) (SingleAssertionFailure (AssertionFailureDiff _ (Actual actual))) =
  return $ Just (path, serializeFixture actual)
serializeFailure (TestOutput _ (Inline _)) (SingleAssertionFailure (AssertionFailureDiff _ (Actual actual))) =
  throwIO $ CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture actual)
serializeFailure (TestOutput _ _) (SingleAssertionFailure (AssertionFailureContains _ (Actual actual))) =
  throwIO $ CouldNotBlessContainsAssertion (fixtureName @a) (serializeFixture actual)
serializeFailure (TestOutput _ (FileLocation path)) (SingleAssertionFailure (AssertionFailureExpectedFileError _ (Actual actual))) =
  return $ Just (path, serializeFixture actual)
serializeFailure (TestOutput _ (Inline _)) (SingleAssertionFailure (AssertionFailureExpectedFileError _ (Actual actual))) =
  throwIO $ CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture actual)
serializeFailure (TestOutput _ _) (SingleAssertionFailure (AssertionFailureActualFileError _)) =
  return Nothing
serializeFailure _ (MultipleAssertionFailures _) =
  throwIO $ CouldNotBlessWithMultipleValues (fixtureName @a)

failed :: Applicative f => Test -> SmokeBlessError -> f TestResult
failed test = pure . TestError test . BlessError
