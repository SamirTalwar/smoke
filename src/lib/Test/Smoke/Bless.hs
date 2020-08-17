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
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types

blessResult :: ResolvedPath Dir -> TestResult -> IO TestResult
blessResult location (TestResult test (TestFailure _ status stdOut stdErr files))
  | isFailureWithMultipleExpectedValues status =
    return $
      TestResult test $
        TestError (BlessError (CouldNotBlessWithMultipleValues "status"))
  | isFailureWithMultipleExpectedValues stdOut =
    return $
      TestResult test $
        TestError (BlessError (CouldNotBlessWithMultipleValues "stdout"))
  | isFailureWithMultipleExpectedValues stdErr =
    return $
      TestResult test $
        TestError (BlessError (CouldNotBlessWithMultipleValues "stderr"))
  | any isFailureWithMultipleExpectedValues (Map.elems files) =
    return $
      TestResult test $
        TestError (BlessError (CouldNotBlessWithMultipleValues "files"))
  | otherwise =
    do
      writeFixture location (testStatus test) status
      writeFixtures location (testStdOut test) stdOut
      writeFixtures location (testStdErr test) stdErr
      forM_ (Map.toList files) $ \(path, fileResult) ->
        writeFixtures location (testFiles test ! path) fileResult
      return $ TestResult test TestSuccess
      `catch` ( \(e :: SmokeBlessError) ->
                  return (TestResult test $ TestError $ BlessError e)
              )
      `catch` (return . TestResult test . TestError . BlessError . BlessIOException)
blessResult _ result = return result

writeFixture :: forall a. FixtureType a => ResolvedPath Dir -> Fixture a -> PartResult a -> IO ()
writeFixture _ _ PartSuccess =
  return ()
writeFixture _ (Fixture (Inline _) _) (PartFailure results) =
  throwIO $
    CouldNotBlessInlineFixture (fixtureName @a) (serializeFixture (assertFailureActual (Vector.head results)))
writeFixture location (Fixture (FileLocation path) _) (PartFailure results) =
  writeToPath (location </> path) (serializeFixture (assertFailureActual (Vector.head results)))

writeFixtures ::
  forall a.
  FixtureType a =>
  ResolvedPath Dir ->
  Fixtures a ->
  PartResult a ->
  IO ()
writeFixtures _ _ PartSuccess =
  return ()
writeFixtures location (Fixtures fixtures) results
  | Vector.length fixtures == 1 =
    writeFixture location (Vector.head fixtures) results
  | Vector.length fixtures == 0 =
    throwIO $ CouldNotBlessAMissingValue (fixtureName @a)
  | otherwise =
    throwIO $ CouldNotBlessWithMultipleValues (fixtureName @a)

isFailureWithMultipleExpectedValues :: PartResult a -> Bool
isFailureWithMultipleExpectedValues (PartFailure results) =
  Vector.length results > 1
isFailureWithMultipleExpectedValues _ = False
