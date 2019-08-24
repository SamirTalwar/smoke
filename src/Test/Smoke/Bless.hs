{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Bless
  ( blessResult
  ) where

import Control.Exception (catch, throwIO)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Path
import Test.Smoke.Maps
import Test.Smoke.Paths
import Test.Smoke.Types

blessResult :: Path Abs Dir -> TestResult -> IO TestResult
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
    do case status of
         PartFailure comparisons ->
           writeFixture
             location
             (testStatus test)
             (snd (Vector.head comparisons))
         _ -> return ()
       case stdOut of
         PartFailure comparisons ->
           writeFixtures
             location
             (testStdOut test)
             (snd (Vector.head comparisons))
         _ -> return ()
       case stdErr of
         PartFailure comparisons ->
           writeFixtures
             location
             (testStdErr test)
             (snd (Vector.head comparisons))
         _ -> return ()
       forWithKeyM_ files $ \path fileResult ->
         case fileResult of
           PartFailure comparisons ->
             writeFixtures
               location
               (testFiles test ! path)
               (snd (Vector.head comparisons))
           _ -> return ()
       return $ TestResult test TestSuccess
     `catch` (\(e :: SmokeBlessError) ->
                return (TestResult test $ TestError $ BlessError e)) `catch`
    (return . TestResult test . TestError . BlessError . BlessIOException)
  where
    isFailureWithMultipleExpectedValues (PartFailure comparisons) =
      Vector.length comparisons > 1
    isFailureWithMultipleExpectedValues _ = False
blessResult _ result = return result

writeFixture :: FixtureType a => Path Abs Dir -> Fixture a -> a -> IO ()
writeFixture _ (Fixture contents@(Inline _) _) value =
  throwIO $
  CouldNotBlessInlineFixture (fixtureName contents) (serializeFixture value)
writeFixture location (Fixture (FileLocation path) _) value =
  writeToPath (location </> path) (serializeFixture value)

writeFixtures ::
     forall a. FixtureType a
  => Path Abs Dir
  -> Fixtures a
  -> a
  -> IO ()
writeFixtures location (Fixtures fixtures) value
  | Vector.length fixtures == 1 =
    writeFixture location (Vector.head fixtures) value
  | Vector.length fixtures == 0 =
    throwIO $ CouldNotBlessAMissingValue (fixtureName (undefined :: Contents a))
  | otherwise =
    throwIO $
    CouldNotBlessWithMultipleValues (fixtureName (undefined :: Contents a))
