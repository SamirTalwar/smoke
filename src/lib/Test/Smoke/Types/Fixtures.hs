{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Types.Fixtures where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Types.Base

data Fixture a
  = Fixture (Contents a) (Maybe Command)
  deriving (Eq, Show)

newtype Fixtures a
  = Fixtures (Vector (Fixture a))
  deriving (Eq, Show)

noFixtures :: Fixtures a
noFixtures = Fixtures Vector.empty

class FixtureType a where
  fixtureName :: Contents a -> FixtureName
  serializeFixture :: a -> Text
  deserializeFixture :: Text -> a

instance FixtureType Text where
  fixtureName = const "text"
  serializeFixture = id
  deserializeFixture = id

instance FixtureType Status where
  fixtureName = const "exit-status"
  serializeFixture = Text.pack . show . unStatus
  deserializeFixture = Status . read . Text.unpack

instance FixtureType StdIn where
  fixtureName = const "stdin"
  serializeFixture = unStdIn
  deserializeFixture = StdIn

instance FixtureType StdOut where
  fixtureName = const "stdout"
  serializeFixture = unStdOut
  deserializeFixture = StdOut

instance FixtureType StdErr where
  fixtureName = const "stderr"
  serializeFixture = unStdErr
  deserializeFixture = StdErr

instance FixtureType a => FromJSON (Fixture a) where
  parseJSON value@(String _) =
    Fixture <$> parseFixtureTypeContents value <*> pure Nothing
  parseJSON value@(Object v) =
    Fixture <$> parseFixtureTypeContents value <*> v .:? "filter"
  parseJSON invalid = typeMismatch "fixture" invalid

instance FixtureType a => FromJSON (Fixtures a) where
  parseJSON (Array v) = Fixtures <$> mapM parseJSON v
  parseJSON v = Fixtures . return <$> (parseJSON v :: Parser (Fixture a))

parseFixtureTypeContents :: FixtureType a => Value -> Parser (Contents a)
parseFixtureTypeContents = parseContents deserializeFixture
