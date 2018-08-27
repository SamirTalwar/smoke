{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Types.Fixtures where

import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Test.Smoke.Types.Base

data Fixture a
  = InlineFixture a
  | FileFixture FilePath
  deriving (Eq, Show)

newtype Fixtures a =
  Fixtures [Fixture a]
  deriving (Eq, Show)

class FixtureContents a where
  fixtureName :: a -> String
  serializeFixture :: a -> Contents
  deserializeFixture :: Contents -> a

instance FixtureContents Status where
  fixtureName = const "exit-status"
  serializeFixture = Text.pack . show . unStatus
  deserializeFixture = Status . read . Text.unpack

instance FixtureContents StdIn where
  fixtureName = const "stdin"
  serializeFixture = unStdIn
  deserializeFixture = StdIn

instance FixtureContents StdOut where
  fixtureName = const "stdout"
  serializeFixture = unStdOut
  deserializeFixture = StdOut

instance FixtureContents StdErr where
  fixtureName = const "stderr"
  serializeFixture = unStdErr
  deserializeFixture = StdErr

instance FixtureContents a => FromJSON (Fixture a) where
  parseJSON (String contents) =
    return $ InlineFixture (deserializeFixture contents)
  parseJSON (Object v) = FileFixture <$> v .: "file"
  parseJSON invalid = typeMismatch "Fixture" invalid

instance FixtureContents a => FromJSON (Fixtures a) where
  parseJSON (Array v) = Fixtures <$> mapM parseJSON (Vector.toList v)
  parseJSON v = Fixtures . return <$> (parseJSON v :: Parser (Fixture a))
