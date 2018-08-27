{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Bless
  ( blessResults
  ) where

import Control.Exception (catch, throwIO)
import qualified Data.Text.IO as TextIO
import Test.Smoke.Types

blessResults :: TestResults -> IO TestResults
blessResults = mapM blessResult

blessResult :: TestResult -> IO TestResult
blessResult (TestFailure TestExecutionPlan {planTest = test} _ (PartFailure (_:_:_) _) _) =
  return $ TestError test (CouldNotBlessWithMultipleValues "stdout")
blessResult (TestFailure TestExecutionPlan {planTest = test} _ _ (PartFailure (_:_:_) _)) =
  return $ TestError test (CouldNotBlessWithMultipleValues "stderr")
blessResult (TestFailure TestExecutionPlan {planTest = test} status stdOut stdErr) =
  do case status of
       PartFailure _ actual -> writeFixture (testStatus test) actual
       _ -> return ()
     case stdOut of
       PartFailure _ actual -> writeFixtures (testStdOut test) actual
       _ -> return ()
     case stdErr of
       PartFailure _ actual -> writeFixtures (testStdErr test) actual
       _ -> return ()
     return $ TestSuccess test
     `catch` \e -> return (TestError test (BlessingFailed e))
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
writeFixtures (Fixtures [fixture]) value = writeFixture fixture value
writeFixtures Fixtures {} _ =
  throwIO $ CouldNotBlessWithMultipleValues (fixtureName (undefined :: a))
