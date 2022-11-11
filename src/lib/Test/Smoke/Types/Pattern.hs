module Test.Smoke.Types.Pattern
  ( Pattern,
    PatternOption (..),
    patternWithOptions,
    matches,
  )
where

import Data.Aeson (FromJSON (..))
import qualified Data.Maybe as Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.ICU as ICU
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
  deserializeFixture = Pattern . ICU.regex []

instance FromFixture Pattern where
  fixtureName = FixtureName "matches"
  serializeFixture (Pattern regex) = ICU.pattern regex

instance FromJSON Pattern where
  parseJSON = parseFixtureJSON "pattern"

matches :: Pattern -> Text -> Bool
matches (Pattern regex) text = Maybe.isJust $ ICU.find regex text
