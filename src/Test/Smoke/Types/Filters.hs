module Test.Smoke.Types.Filters where

import Data.Aeson
import Data.Text (Text)
import Test.Smoke.Types.Base

newtype FixtureFilter =
  FixtureFilter (Contents Text)
  deriving (Eq, Show)

instance FromJSON FixtureFilter where
  parseJSON value = FixtureFilter <$> parseContents id value

data Filtered a
  = Unfiltered a
  | Filtered a
             FixtureFilter
  deriving (Eq, Show)

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
