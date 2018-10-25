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
import Test.Smoke.Types.Paths

newtype Fixture a =
  Fixture (Contents a)
  deriving (Eq, Show)

newtype Fixtures a =
  Fixtures (Vector (Fixture a))
  deriving (Eq, Show)

noFixtures :: Fixtures a
noFixtures = Fixtures Vector.empty

data Contents a
  = Inline a
  | FileLocation Path
  deriving (Eq, Show)

class FixtureType a where
  fixtureName :: a -> String
  serializeFixture :: a -> Text
  deserializeFixture :: Text -> a

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
  parseJSON (String contents) =
    return $ Fixture $ Inline $ deserializeFixture contents
  parseJSON (Object v) = Fixture . FileLocation <$> v .: "file"
  parseJSON invalid = typeMismatch "fixture" invalid

instance FixtureType a => FromJSON (Fixtures a) where
  parseJSON (Array v) = Fixtures <$> mapM parseJSON v
  parseJSON v = Fixtures . return <$> (parseJSON v :: Parser (Fixture a))
