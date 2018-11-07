module Test.Smoke.Types.Filters where

import Data.Aeson
import Data.Text (Text)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Paths

data FixtureFilter
  = InlineFixtureFilter Text
  | CommandFixtureFilter Path
  deriving (Eq, Show)

instance FromJSON FixtureFilter where
  parseJSON value = do
    contents <- parseContents id value
    return $
      case contents of
        Inline script -> InlineFixtureFilter script
        FileLocation path -> CommandFixtureFilter path

data Filtered a
  = Unfiltered a
  | Filtered a
             FixtureFilter
  deriving (Eq, Show)

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
