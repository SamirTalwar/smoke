{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Smoke.Bless
  ( blessResult,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad (forM_)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types

blessResult :: ResolvedPath Dir -> TestResult -> IO TestResult
blessResult _ (TestResult test (TestFailure _ (EqualityFailure _ (Status actualStatus)) _ _ _)) =
  failed test $ CouldNotBlessInlineFixture "status" (Text.pack (show actualStatus))
blessResult location (TestResult test (TestFailure _ _ stdOut stdErr files)) =
  do
    serializedStdOut <- serialize (testStdOut test) stdOut
    serializedStdErr <- serialize (testStdErr test) stdErr
    serializedFiles <- Map.traverseWithKey (\path fileResult -> serialize (testFiles test ! path) fileResult) files
    writeFixture location serializedStdOut
    writeFixture location serializedStdErr
    forM_ serializedFiles $ writeFixture location
    return $ TestResult test TestSuccess
    `catch` (\(e :: SmokeBlessError) -> failed test e)
    `catch` (failed test . BlessIOException)
blessResult _ result = return result

serialize :: forall a. FixtureType a => Vector (TestOutput a) -> AssertionResult a -> IO (Maybe (RelativePath File, Text))
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

serializeFailure :: forall a. FixtureType a => TestOutput a -> AssertionFailures a -> IO (Maybe (RelativePath File, Text))
serializeFailure (TestOutput _ (FileLocation path)) (SingleAssertionFailure (AssertionFailureDiff _ actual)) =
  return $ Just (path, serializeFixture actual)
serializeFailure (TestOutput _ (Inline _)) (SingleAssertionFailure (AssertionFailureDiff _ actual)) =
  throwIO $ CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture actual)
serializeFailure (TestOutput _ _) (SingleAssertionFailure (AssertionFailureContains _ actual)) =
  throwIO $ CouldNotBlessContainsAssertion (fixtureName @a) (serializeFixture actual)
serializeFailure (TestOutput _ _) (SingleAssertionFailure (AssertionFailureFileError fileError)) =
  throwIO $ CouldNotBlessAssertionFileError (fixtureName @a) fileError
serializeFailure _ (MultipleAssertionFailures _) =
  throwIO $ CouldNotBlessWithMultipleValues (fixtureName @a)

writeFixture :: ResolvedPath Dir -> Maybe (RelativePath File, Text) -> IO ()
writeFixture _ Nothing =
  return ()
writeFixture location (Just (path, text)) =
  writeToPath (location </> path) text

failed :: Applicative f => Test -> SmokeBlessError -> f TestResult
failed test = pure . TestResult test . TestError . BlessError
