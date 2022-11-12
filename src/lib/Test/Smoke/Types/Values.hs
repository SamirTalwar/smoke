{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Types.Values (TestInput (..), TestOutput (..)) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Filters

data TestInput a where
  TestInputInline :: a -> TestInput a
  TestInputFromFile :: Path Relative File -> TestInput a
  TestInputFiltered :: TestInput a -> Filter -> TestInput a

instance FromJSON a => FromJSON (TestInput a) where
  parseJSON value = do
    (inner, contentFilter) <- parseContentsOrFile value
    let testInput = case inner of
          Left contents -> TestInputInline contents
          Right file -> TestInputFromFile file
    pure $ maybe testInput (TestInputFiltered testInput) contentFilter

data TestOutput actual where
  TestOutputInline :: Assert actual -> TestOutput actual
  TestOutputFromFile :: ToFixture expected => (expected -> Assert actual) -> Path Relative File -> TestOutput actual

instance (Eq actual, ToFixture actual, FromFixture actual, FromJSON actual) => FromJSON (TestOutput actual) where
  parseJSON value@(Object v) =
    let contents :: Parser Object =
          v .: "contents"
        equals :: Parser (TestOutput actual) = do
          expected <- v .: "equals" <|> (contents >>= (.: "equals"))
          parseTestOutput AssertEquals expected
        contains :: Parser (TestOutput actual) = do
          expected <- v .: "contains" <|> (contents >>= (.: "contains"))
          parseTestOutput AssertContains expected
        matches :: Parser (TestOutput actual) = do
          expected <- v .: "matches" <|> (contents >>= (.: "matches"))
          parseTestOutput AssertMatches expected
        fallback :: Parser (TestOutput actual) =
          parseTestOutput AssertEquals value
     in equals <|> contains <|> matches <|> fallback
  parseJSON value =
    parseTestOutput AssertEquals value

parseTestOutput ::
  (ToFixture expected, FromJSON expected, ToFixture actual, FromFixture actual, FromJSON actual) =>
  (expected -> Assert actual) ->
  Value ->
  Parser (TestOutput actual)
parseTestOutput assertion value = do
  (inner, contentFilter) <- parseContentsOrFile value
  let assertion' = maybe assertion filteredAssertion contentFilter
  pure $ case inner of
    Left contents -> TestOutputInline (assertion' contents)
    Right file -> TestOutputFromFile assertion' file
  where
    filteredAssertion fixtureFilter expected = AssertFiltered fixtureFilter (assertion expected)

parseContentsOrFile :: FromJSON a => Value -> Parser (Either a (Path Relative File), Maybe Filter)
parseContentsOrFile s@(String _) = do
  contents <- parseJSON s
  pure (Left contents, Nothing)
parseContentsOrFile (Object v) = do
  contentFilter <- v .:? "filter"
  maybeContents <- v .:? "contents"
  maybeFile <- v .:? "file"
  case (maybeContents, maybeFile) of
    (Just _, Just _) -> fail "Expected \"contents\" or a \"file\", not both"
    (Nothing, Nothing) -> fail "Expected \"contents\" or a \"file\""
    (Just contents, Nothing) -> pure (Left contents, contentFilter)
    (Nothing, Just file) -> pure (Right file, contentFilter)
parseContentsOrFile invalid =
  typeMismatch "String, or Object containing \"contents\" or a \"file\"" invalid
