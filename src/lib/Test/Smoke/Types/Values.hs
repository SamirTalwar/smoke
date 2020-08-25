{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Types.Values (Contents (..), TestInput (..), TestOutput (..)) where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Filters

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

data TestInput a = TestInput
  { testInputFilter :: Maybe Filter,
    testInputContents :: Contents a
  }

instance FromJSON a => FromJSON (TestInput a) where
  parseJSON value@(Object v) = TestInput <$> v .:? "filter" <*> parseJSON value
  parseJSON value = TestInput Nothing <$> parseJSON value

data TestOutput a = TestOutput
  { testOutputAssertionConstructor :: a -> Assert a,
    testOutputContents :: Contents a
  }

instance (Eq a, FromJSON a) => FromJSON (TestOutput a) where
  parseJSON value@(Object v) =
    let equals :: Parser (Maybe (TestOutput a)) = (v .:? "equals") >>= (sequence . (parseFiltered AssertEqual <$>))
        fallback :: Parser (TestOutput a) = parseFiltered AssertEqual value
     in equals `orDefinitely` fallback
  parseJSON value =
    parseFiltered AssertEqual value

parseFiltered :: (Eq a, FromJSON a) => (a -> Assert a) -> Value -> Parser (TestOutput a)
parseFiltered assertion value@(Object v) =
  TestOutput
    <$> (maybe assertion (\f expected -> AssertFiltered f (assertion expected)) <$> v .:? "filter")
    <*> parseJSON value
parseFiltered assertion value =
  TestOutput assertion <$> parseJSON value

orDefinitely :: Monad m => m (Maybe a) -> m a -> m a
a `orDefinitely` b = do
  aValue <- a
  maybe b return aValue
