{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Tests where

import Data.Aeson
import Data.Default (def)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Values

data TestSpecification
  = TestSpecification (Maybe Command) Suites

type Suites = [SuiteWithMetadata]

data SuiteWithMetadata = SuiteWithMetadata
  { suiteMetaName :: SuiteName,
    suiteMetaLocation :: Path Resolved Dir,
    suiteMetaSuite :: Suite
  }

data Suite = Suite
  { suiteWorkingDirectory :: Maybe (Path Relative Dir),
    suiteShell :: Maybe CommandLine,
    suiteCommand :: Maybe Command,
    suiteTests :: [Test]
  }

instance FromJSON Suite where
  parseJSON =
    withObject "Suite" $ \v ->
      Suite
        <$> (v .:? "working-directory")
        <*> (v .:? "shell")
        <*> (v .:? "command")
        <*> (v .: "tests")

data Test = Test
  { testName :: TestName,
    testIgnored :: Bool,
    testWorkingDirectory :: Maybe (Path Relative Dir),
    testCommand :: Maybe Command,
    testArgs :: Maybe Args,
    testEnvironment :: Maybe EnvVars,
    testStdIn :: Maybe (TestInput StdIn),
    testStatus :: Status,
    testStdOut :: Vector (TestOutput StdOut),
    testStdErr :: Vector (TestOutput StdErr),
    testFiles :: Map (Path Relative File) (Vector (TestOutput TestFileContents)),
    testRevert :: Vector (Path Relative Dir)
  }

instance FromJSON Test where
  parseJSON =
    withObject "Test" $ \v ->
      Test
        <$> (TestName <$> v .: "name")
        <*> (v .:? "ignored" .!= False)
        <*> (v .:? "working-directory")
        <*> (v .:? "command")
        <*> (v .:? "args")
        <*> (v .:? "environment")
        <*> (v .:? "stdin")
        <*> (v .:? "exit-status" .!= def)
        <*> (manyMaybe <$> (v .:? "stdout"))
        <*> (manyMaybe <$> (v .:? "stderr"))
        <*> ( Map.fromList . map (\(TestFile path contents) -> (path, contents)) . Vector.toList
                <$> (v .:? "files" .!= Vector.empty)
            )
        <*> (v .:? "revert" .!= Vector.empty)

data TestFile = TestFile
  { testFilePath :: Path Relative File,
    testFileContents :: Vector (TestOutput TestFileContents)
  }

instance FromJSON TestFile where
  parseJSON =
    withObject "TestFile" $ \v ->
      TestFile <$> (v .: "path") <*> (unMany <$> (v .: "contents"))

newtype Many a = Many {unMany :: Vector a}

instance FromJSON a => FromJSON (Many a) where
  parseJSON a@(Array _) = Many <$> parseJSON a
  parseJSON v = Many . Vector.singleton <$> parseJSON v

manyMaybe :: FromJSON a => Maybe (Many a) -> Vector a
manyMaybe Nothing = Vector.empty
manyMaybe (Just v) = unMany v
