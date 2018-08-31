{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Bless
  ( blessResults
  ) where

import Control.Exception (catch, throwIO)
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as Vector
import Test.Smoke.Types

blessResults :: TestResults -> IO TestResults
blessResults = mapM blessResult

blessResult :: TestResult -> IO TestResult
blessResult (TestFailure name TestExecutionPlan {planTest = test} status stdOut stdErr)
  | isFailureWithMultipleExpectedValues stdOut =
    return $
    TestError name (BlessError (CouldNotBlessWithMultipleValues "stdout"))
  | isFailureWithMultipleExpectedValues stdErr =
    return $
    TestError name (BlessError (CouldNotBlessWithMultipleValues "stderr"))
  | otherwise =
    do case status of
         PartFailure _ actual -> writeFixture (testStatus test) actual
         _ -> return ()
       case stdOut of
         PartFailure _ actual -> writeFixtures (testStdOut test) actual
         _ -> return ()
       case stdErr of
         PartFailure _ actual -> writeFixtures (testStdErr test) actual
         _ -> return ()
       return $ TestSuccess name
     `catch` (\(e :: TestBlessErrorMessage) ->
                return (TestError name (BlessError e))) `catch`
    (return . TestError name . BlessIOException)
  where
    isFailureWithMultipleExpectedValues (PartFailure expected _) =
      Vector.length expected > 1
    isFailureWithMultipleExpectedValues _ = False
blessResult result = return result

writeFixture :: FixtureContents a => Fixture a -> a -> IO ()
writeFixture (InlineFixture contents) _ =
  throwIO $
  CouldNotWriteFixture (fixtureName contents) (serializeFixture contents)
writeFixture (FileFixture path) value =
  TextIO.writeFile path (serializeFixture value)

writeFixtures ::
     forall a. FixtureContents a
  => Fixtures a
  -> a
  -> IO ()
writeFixtures (Fixtures fixtures) value
  | Vector.length fixtures == 1 = writeFixture (Vector.head fixtures) value
  | otherwise =
    throwIO $ CouldNotBlessWithMultipleValues (fixtureName (undefined :: a))
