{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Types.Values (Contents (..), TestInput (..), TestOutput (..)) where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Filters

data Contents a where
  Inline :: a -> Contents a
  FileLocation :: FixtureType a => Path Relative File -> Contents a

instance (FixtureType a, FromJSON a) => FromJSON (Contents a) where
  parseJSON s@(String _) =
    Inline <$> parseJSON s
  parseJSON (Object v) = do
    maybeContents <- v .:? "contents"
    maybeFile <- v .:? "file"
    case (maybeContents, maybeFile) of
      (Just _, Just _) -> fail "Expected \"contents\" or a \"file\", not both"
      (Just contents, Nothing) -> Inline <$> parseJSON contents
      (Nothing, Just file) -> return $ FileLocation file
      (Nothing, Nothing) -> fail "Expected \"contents\" or a \"file\""
  parseJSON invalid = typeMismatch "\"contents\" or a \"file\"" invalid

data TestInput a where
  TestInput :: Contents a -> TestInput a
  TestInputFiltered :: FixtureType a => Filter -> Contents a -> TestInput a

instance (FixtureType a, FromJSON a) => FromJSON (TestInput a) where
  parseJSON value@(Object v) =
    maybe TestInput TestInputFiltered <$> v .:? "filter" <*> parseJSON value
  parseJSON value =
    TestInput <$> parseJSON value

data TestOutput actual = forall expected.
  TestOutput
  { testOutputAssertionConstructor :: expected -> Assert actual,
    testOutputContents :: Contents expected
  }

instance (FixtureType actual, FromJSON actual) => FromJSON (TestOutput actual) where
  parseJSON value@(Object v) =
    let equals :: Parser (Maybe (TestOutput actual)) = do
          expected <- v .:? "equals"
          sequence $ parseFiltered AssertEquals <$> expected
        contains :: Parser (Maybe (TestOutput actual)) = do
          expected <- v .:? "contains"
          sequence $ parseFiltered AssertContains <$> expected
        fallback :: Parser (TestOutput actual) = parseFiltered AssertEquals value
     in equals `orMaybe` contains `orDefinitely` fallback
  parseJSON value =
    parseFiltered AssertEquals value

parseFiltered :: (FixtureType expected, FromJSON expected, FixtureType actual) => (expected -> Assert actual) -> Value -> Parser (TestOutput actual)
parseFiltered assertion value@(Object v) =
  TestOutput
    <$> (maybe assertion filteredAssertion <$> v .:? "filter")
    <*> parseJSON value
  where
    filteredAssertion fixtureFilter expected = AssertFiltered fixtureFilter (assertion expected)
parseFiltered assertion value =
  TestOutput assertion <$> parseJSON value

orMaybe :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
a `orMaybe` b = do
  aValue <- a
  maybe b (return . Just) aValue

orDefinitely :: Monad m => m (Maybe a) -> m a -> m a
a `orDefinitely` b = do
  aValue <- a
  maybe b return aValue
