{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Parse (parseSuite) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
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
      <*> (manyMaybe =<< (v .:? "stdout"))
      <*> (manyMaybe =<< (v .:? "stderr"))
      <*> ( Fixture . Inline <$> (v .:? "exit-status" .!= def)
              <*> return Nothing
          )
      <*> ( Map.fromList . Vector.toList
              <$> (Vector.mapM parseTestFile =<< (v .:? "files" .!= Vector.empty))
          )
      <*> (v .:? "revert" .!= Vector.empty)

parseTestFile :: Value -> Parser (RelativePath File, Vector (Fixture TestFileContents))
parseTestFile =
  withObject "File" $ \v -> do
    path <- parseFile <$> v .: "path"
    contents <- many =<< (v .: "contents")
    return (path, contents)

toWorkingDirectory ::
  ResolvedPath Dir -> Maybe (RelativePath Dir) -> Maybe WorkingDirectory
toWorkingDirectory location path = WorkingDirectory . (location </>) <$> path

many :: FromJSON a => Value -> Parser (Vector a)
many (Array v) = mapM parseJSON v
many v = Vector.singleton <$> parseJSON v

manyMaybe :: FromJSON a => Maybe Value -> Parser (Vector a)
manyMaybe (Just v) = many v
manyMaybe Nothing = return Vector.empty
