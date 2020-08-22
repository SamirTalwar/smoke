{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Fixtures where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Test.Smoke.Paths
import Test.Smoke.Types.Base

data Contents a
  = Inline a
  | FileLocation (RelativePath File)

instance FromJSON a => FromJSON (Contents a) where
  parseJSON s@(String _) =
    Inline <$> parseJSON s
  parseJSON (Object v) = do
    maybeContents <- v .:? "contents"
    maybeFile <- v .:? "file"
    case (maybeContents, maybeFile) of
      (Just _, Just _) -> fail "Expected \"contents\" or a \"file\", not both."
      (Just contents, Nothing) -> Inline <$> parseJSON contents
      (Nothing, Just file) -> return $ FileLocation file
      (Nothing, Nothing) -> fail "Expected \"contents\" or a \"file\"."
  parseJSON invalid = typeMismatch "contents" invalid

data Fixture a
  = Fixture (Contents a) (Maybe Command)

instance (FromJSON a, FixtureType a) => FromJSON (Fixture a) where
  parseJSON value@(String _) =
    Fixture <$> parseJSON value <*> pure Nothing
  parseJSON value@(Object v) =
    Fixture <$> parseJSON value <*> v .:? "filter"
  parseJSON invalid = typeMismatch "fixture" invalid
