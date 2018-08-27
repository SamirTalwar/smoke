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
blessResult (TestFailure name _ _ (PartFailure (_:_:_) _) _) =
  return $ TestError name (CouldNotBlessWithMultipleValues "stdout")
blessResult (TestFailure name _ _ _ (PartFailure (_:_:_) _)) =
  return $ TestError name (CouldNotBlessWithMultipleValues "stderr")
blessResult (TestFailure name TestExecutionPlan {planTest = test} status stdOut stdErr) =
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
     `catch` \e -> return (TestError name (BlessingFailed e))
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
