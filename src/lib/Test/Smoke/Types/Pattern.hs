{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Pattern
  ( Pattern,
    PatternOption (..),
    patternWithOptions,
    matches,
  )
where

import Data.Aeson (FromJSON (..), (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Maybe qualified as Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.ICU qualified as ICU
import Test.Smoke.Types.Base

newtype Pattern = Pattern ICU.Regex

data PatternOption = CaseInsensitive | Comments | DotAll

patternWithOptions :: [PatternOption] -> Text -> Pattern
patternWithOptions options = Pattern . ICU.regex (map optionsToICU options)
  where
    optionsToICU :: PatternOption -> ICU.MatchOption
    optionsToICU CaseInsensitive = ICU.CaseInsensitive
    optionsToICU Comments = ICU.Comments
    optionsToICU DotAll = ICU.DotAll

instance IsString Pattern where
  fromString = Pattern . fromString

instance ToFixture Pattern where
  deserializeFixture = patternWithOptions []

instance FromFixture Pattern where
  fixtureName = FixtureName "pattern"
  serializeFixture (Pattern regex) = ICU.pattern regex

instance FromJSON Pattern where
  parseJSON (Aeson.String value) = pure $ patternWithOptions [] value
  parseJSON (Aeson.Object properties) = do
    regex <- properties .: "regex"
    options <- properties .:? "options" .!= []
    pure $ patternWithOptions options regex
  parseJSON invalid = Aeson.typeMismatch "String or Object" invalid

instance FromJSON PatternOption where
  parseJSON = Aeson.withText "option" $ \case
    "case-insensitive" -> pure CaseInsensitive
    "i" -> pure CaseInsensitive
    "comments" -> pure Comments
    "x" -> pure Comments
    "dot-all" -> pure DotAll
    "s" -> pure DotAll
    invalid -> fail $ "Expected one of [\"case-insensitive\" / \"i\", \"comments\" / \"x\", \"dot-all\" / \"s\"], but got " <> show invalid

matches :: Pattern -> Text -> Bool
matches (Pattern regex) text = Maybe.isJust $ ICU.find regex text
