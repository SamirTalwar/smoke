{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Bless
  ( blessResults
  ) where

import Control.Exception (catch, throwIO)
import Control.Monad (forM)
import qualified Data.Vector as Vector
import Test.Smoke.Files
import Test.Smoke.Types

blessResults :: Results -> IO Results
blessResults results =
  forM results $ \result@(SuiteResult suiteName suiteResult) ->
    case suiteResult of
      Left _ -> return result
      Right testResults -> do
        blessedResults <- forM testResults blessResult
        return $ SuiteResult suiteName (Right blessedResults)

blessResult :: TestResult -> IO TestResult
blessResult (TestResult test (TestFailure _ status stdOut stdErr))
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
  | otherwise =
    do case status of
         PartFailure comparisons ->
           writeFixture (testStatus test) (snd (Vector.head comparisons))
         _ -> return ()
       case stdOut of
         PartFailure comparisons ->
           writeFixtures (testStdOut test) (snd (Vector.head comparisons))
         _ -> return ()
       case stdErr of
         PartFailure comparisons ->
           writeFixtures (testStdErr test) (snd (Vector.head comparisons))
         _ -> return ()
       return $ TestResult test TestSuccess
     `catch` (\(e :: SmokeBlessError) ->
                return (TestResult test $ TestError $ BlessError e)) `catch`
    (return . TestResult test . TestError . BlessError . BlessIOException)
  where
    isFailureWithMultipleExpectedValues (PartFailure comparisons) =
      Vector.length comparisons > 1
    isFailureWithMultipleExpectedValues _ = False
blessResult result = return result

writeFixture :: FixtureType a => Fixture a -> a -> IO ()
writeFixture (Fixture contents@(Inline _) _) value =
  throwIO $
  CouldNotBlessInlineFixture (fixtureName contents) (serializeFixture value)
writeFixture (Fixture (FileLocation path) _) value =
  writeToPath path (serializeFixture value)

writeFixtures ::
     forall a. FixtureType a
  => Fixtures a
  -> a
  -> IO ()
writeFixtures (Fixtures fixtures) value
  | Vector.length fixtures == 1 = writeFixture (Vector.head fixtures) value
  | Vector.length fixtures == 0 =
    throwIO $ CouldNotBlessAMissingValue (fixtureName (undefined :: Contents a))
  | otherwise =
    throwIO $
    CouldNotBlessWithMultipleValues (fixtureName (undefined :: Contents a))
