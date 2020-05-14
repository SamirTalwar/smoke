{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Tests where

import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Maps
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Files
import Test.Smoke.Types.Fixtures

data TestSpecification
  = TestSpecification (Maybe Command) Suites

type Suites = [(SuiteName, Either SmokeDiscoveryError Suite)]

data Suite
  = Suite
      { suiteLocation :: ResolvedPath Dir,
        suiteWorkingDirectory :: Maybe WorkingDirectory,
        suiteShell :: Maybe CommandLine,
        suiteCommand :: Maybe Command,
        suiteTests :: [Test]
      }
  deriving (Eq, Show)

data Test
  = Test
      { testName :: TestName,
        testIgnored :: Bool,
        testWorkingDirectory :: Maybe WorkingDirectory,
        testCommand :: Maybe Command,
        testArgs :: Maybe Args,
        testStdIn :: Maybe (Fixture StdIn),
        testStdOut :: Fixtures StdOut,
        testStdErr :: Fixtures StdErr,
        testStatus :: Fixture Status,
        testFiles :: Map (RelativePath File) (Fixtures TestFileContents),
        testRevert :: Vector (RelativePath Dir)
      }
  deriving (Eq, Show)

parseSuite :: ResolvedPath Dir -> Value -> Parser Suite
parseSuite location =
  withObject "Suite" $ \v ->
    Suite location
      <$> (toWorkingDirectory location <$> (v .:? "working-directory"))
      <*> (v .:? "shell")
      <*> (v .:? "command")
      <*> (mapM (parseTest location) =<< (v .: "tests"))

parseTest :: ResolvedPath Dir -> Value -> Parser Test
parseTest location =
  withObject "Test" $ \v ->
    Test <$> (TestName <$> v .: "name")
      <*> (v .:? "ignored" .!= False)
      <*> (toWorkingDirectory location <$> (v .:? "working-directory"))
      <*> (v .:? "command")
      <*> (v .:? "args")
      <*> (v .:? "stdin")
      <*> (v .:? "stdout" .!= noFixtures)
      <*> (v .:? "stderr" .!= noFixtures)
      <*> ( Fixture <$> (Inline . Status <$> v .:? "exit-status" .!= 0)
              <*> return Nothing
          )
      <*> ( mapFromTraversable
              <$> (Vector.mapM parseTestFile =<< (v .:? "files" .!= Vector.empty))
          )
      <*> (v .:? "revert" .!= Vector.empty)

toWorkingDirectory ::
  ResolvedPath Dir -> Maybe (RelativePath Dir) -> Maybe WorkingDirectory
toWorkingDirectory location path = WorkingDirectory . (location </>) <$> path

parseTestFile :: Value -> Parser (RelativePath File, Fixtures TestFileContents)
parseTestFile =
  withObject "File" $ \v -> do
    path <- parseFile <$> v .: "path"
    contents <- v .: "contents"
    return (path, contents)
