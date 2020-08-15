{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Parse (parseSuite) where

import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Files
import Test.Smoke.Types.Fixtures
import Test.Smoke.Types.Tests

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
      <*> ( Map.fromList . Vector.toList
              <$> (Vector.mapM parseTestFile =<< (v .:? "files" .!= Vector.empty))
          )
      <*> (v .:? "revert" .!= Vector.empty)

parseTestFile :: Value -> Parser (RelativePath File, Fixtures TestFileContents)
parseTestFile =
  withObject "File" $ \v -> do
    path <- parseFile <$> v .: "path"
    contents <- v .: "contents"
    return (path, contents)

toWorkingDirectory ::
  ResolvedPath Dir -> Maybe (RelativePath Dir) -> Maybe WorkingDirectory
toWorkingDirectory location path = WorkingDirectory . (location </>) <$> path
