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
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types

blessResult :: ResolvedPath Dir -> TestResult -> IO TestResult
blessResult _ (TestResult test (TestFailure _ (PartFailure results) _ _ _)) =
  failed test $ couldNotBlessInlineFixture results
blessResult location (TestResult test (TestFailure _ _ stdOut stdErr files))
  | isFailureWithMultipleExpectedValues stdOut =
    failed test $ CouldNotBlessWithMultipleValues (fixtureName @StdOut)
  | isFailureWithMultipleExpectedValues stdErr =
    failed test $ CouldNotBlessWithMultipleValues (fixtureName @StdErr)
  | any isFailureWithMultipleExpectedValues (Map.elems files) =
    failed test $ CouldNotBlessWithMultipleValues "files"
  | otherwise =
    do
      writeFixtures location (testStdOut test) stdOut
      writeFixtures location (testStdErr test) stdErr
      forM_ (Map.toList files) $ \(path, fileResult) ->
        writeFixtures location (testFiles test ! path) fileResult
      return $ TestResult test TestSuccess
      `catch` (\(e :: SmokeBlessError) -> failed test e)
      `catch` (failed test . BlessIOException)
blessResult _ result = return result

writeFixture :: FixtureType a => ResolvedPath Dir -> TestOutput a -> PartResult a -> IO ()
writeFixture _ _ PartSuccess =
  return ()
writeFixture _ (TestOutput _ (Inline _)) (PartFailure results) =
  throwIO $ couldNotBlessInlineFixture results
writeFixture location (TestOutput _ (FileLocation path)) (PartFailure results) =
  writeToPath (location </> path) (serializeFixture (assertFailureActual (Vector.head results)))

writeFixtures ::
  forall a.
  FixtureType a =>
  ResolvedPath Dir ->
  Vector (TestOutput a) ->
  PartResult a ->
  IO ()
writeFixtures _ _ PartSuccess =
  return ()
writeFixtures location outputs results =
  case Vector.length outputs of
    1 ->
      writeFixture location (Vector.head outputs) results
    0 ->
      throwIO $ CouldNotBlessAMissingValue (fixtureName @a)
    _ ->
      throwIO $ CouldNotBlessWithMultipleValues (fixtureName @a)

failed :: Applicative f => Test -> SmokeBlessError -> f TestResult
failed test = pure . TestResult test . TestError . BlessError

couldNotBlessInlineFixture :: forall a. FixtureType a => Vector (AssertFailure a) -> SmokeBlessError
couldNotBlessInlineFixture results =
  CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture (assertFailureActual (Vector.head results)))

isFailureWithMultipleExpectedValues :: PartResult a -> Bool
isFailureWithMultipleExpectedValues (PartFailure results) =
  Vector.length results > 1
isFailureWithMultipleExpectedValues _ = False
