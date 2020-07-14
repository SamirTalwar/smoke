{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Files where

import Data.Aeson
import Data.Text (Text)
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Fixtures

data TestFile = TestFile
  { testFilePath :: RelativePath File,
    testFileContents :: Fixtures TestFileContents
  }
  deriving (Eq, Show)

newtype TestFileContents = TestFileContents
  { unTestFileContents :: Text
  }
  deriving (Eq, Show, FromJSON)

instance FixtureType TestFileContents where
  fixtureName = const "file contents"
  serializeFixture = unTestFileContents
  deserializeFixture = TestFileContents

instance FromJSON TestFile where
  parseJSON =
    withObject "TestFile" $ \v ->
      TestFile <$> (v .: "path") <*> (v .: "contents")

newtype Reversions = Reversions
  { unReversions :: Vector (RelativePath Dir)
  }
  deriving (Eq, Show, FromJSON)
