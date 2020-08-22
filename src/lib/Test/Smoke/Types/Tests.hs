{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Tests where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Values

data TestSpecification
  = TestSpecification (Maybe Command) Suites

type Suites = [SuiteWithMetadata]

data SuiteWithMetadata = SuiteWithMetadata
  { suiteMetaName :: SuiteName,
    suiteMetaLocation :: ResolvedPath Dir,
    suiteMetaSuite :: Suite
  }

data Suite = Suite
  { suiteWorkingDirectory :: Maybe (RelativePath Dir),
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
        <*> (mapM parseJSON =<< (v .: "tests"))

data Test = Test
  { testName :: TestName,
    testIgnored :: Bool,
    testWorkingDirectory :: Maybe (RelativePath Dir),
    testCommand :: Maybe Command,
    testArgs :: Maybe Args,
    testStdIn :: Maybe (TestInput StdIn),
    testStatus :: Assert Status,
    testStdOut :: Vector (TestOutput StdOut),
    testStdErr :: Vector (TestOutput StdErr),
    testFiles :: Map (RelativePath File) (Vector (TestOutput TestFileContents)),
    testRevert :: Vector (RelativePath Dir)
  }

instance FromJSON Test where
  parseJSON =
    withObject "Test" $ \v ->
      Test <$> (TestName <$> v .: "name")
        <*> (v .:? "ignored" .!= False)
        <*> (v .:? "working-directory")
        <*> (v .:? "command")
        <*> (v .:? "args")
        <*> (v .:? "stdin")
        <*> (AssertEqual <$> (v .:? "exit-status" .!= def))
        <*> (manyMaybe =<< (v .:? "stdout"))
        <*> (manyMaybe =<< (v .:? "stderr"))
        <*> ( Map.fromList . map (\(TestFile path contents) -> (path, contents)) . Vector.toList
                <$> (v .:? "files" .!= Vector.empty)
            )
        <*> (v .:? "revert" .!= Vector.empty)

data TestFile = TestFile
  { testFilePath :: RelativePath File,
    testFileContents :: Vector (TestOutput TestFileContents)
  }

instance FromJSON TestFile where
  parseJSON =
    withObject "TestFile" $ \v ->
      TestFile <$> (v .: "path") <*> (many =<< (v .: "contents"))

many :: FromJSON a => Value -> Parser (Vector a)
many (Array v) = mapM parseJSON v
many v = Vector.singleton <$> parseJSON v

manyMaybe :: FromJSON a => Maybe Value -> Parser (Vector a)
manyMaybe = maybe (return Vector.empty) many
