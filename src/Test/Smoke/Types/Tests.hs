{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Tests where

import Control.Monad ((<=<))
import Control.Monad.Catch.Pure (runCatchT)
import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Path
import Test.Smoke.Maps
import Test.Smoke.Paths
import Test.Smoke.Types.Args
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Files
import Test.Smoke.Types.Fixtures

data Options =
  Options
    { optionsCommand :: Maybe Command
    , optionsTestLocations :: Vector String
    }
  deriving (Eq, Show)

data TestSpecification =
  TestSpecification (Maybe Command) Suites

type Suites = [(SuiteName, Either SmokeDiscoveryError Suite)]

data Suite =
  Suite
    { suiteLocation :: Path Abs Dir
    , suiteWorkingDirectory :: Maybe WorkingDirectory
    , suiteCommand :: Maybe Command
    , suiteTests :: [Test]
    }
  deriving (Eq, Show)

data Test =
  Test
    { testName :: TestName
    , testWorkingDirectory :: Maybe WorkingDirectory
    , testCommand :: Maybe Command
    , testArgs :: Maybe Args
    , testStdIn :: Maybe (Fixture StdIn)
    , testStdOut :: Fixtures StdOut
    , testStdErr :: Fixtures StdErr
    , testStatus :: Fixture Status
    , testFiles :: Map (Path Rel File) (Fixtures TestFileContents)
    , testRevert :: Vector (Path Rel Dir)
    }
  deriving (Eq, Show)

parseSuite :: Path Abs Dir -> Value -> Parser Suite
parseSuite location =
  withObject "Suite" $ \v ->
    Suite location <$>
    (parseWorkingDirectory location =<< (v .:? "working-directory")) <*>
    (v .:? "command") <*>
    (mapM (parseTest location) =<< (v .: "tests"))

parseTest :: Path Abs Dir -> Value -> Parser Test
parseTest location =
  withObject "Test" $ \v ->
    Test <$> (TestName <$> v .: "name") <*>
    (parseWorkingDirectory location =<< (v .:? "working-directory")) <*>
    (v .:? "command") <*>
    (v .:? "args") <*>
    (v .:? "stdin") <*>
    (v .:? "stdout" .!= noFixtures) <*>
    (v .:? "stderr" .!= noFixtures) <*>
    (Fixture <$> (Inline . Status <$> v .:? "exit-status" .!= 0) <*>
     return Nothing) <*>
    (mapFromTraversable <$>
     (Vector.mapM parseTestFile =<< (v .:? "files" .!= Vector.empty))) <*>
    (v .:? "revert" .!= Vector.empty)

parseWorkingDirectory ::
     Path Abs Dir -> Maybe FilePath -> Parser (Maybe WorkingDirectory)
parseWorkingDirectory _ Nothing = return Nothing
parseWorkingDirectory location (Just filePath) =
  either (fail . show) (return . Just . WorkingDirectory) $
  location <//> filePath

parseTestFile :: Value -> Parser (Path Rel File, Fixtures TestFileContents)
parseTestFile =
  withObject "File" $ \v -> do
    path <-
      (either (fail . show) return <=< runCatchT . parseAbsOrRelFile) =<<
      v .: "path"
    contents <- v .: "contents"
    return (path, contents)
