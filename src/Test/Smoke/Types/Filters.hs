module Test.Smoke.Types.Filters where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Vector as Vector
import Test.Smoke.Types.Base
import Test.Smoke.Types.Paths

data FixtureFilter
  = InlineFixtureFilter Text
  | CommandFixtureFilter Executable
                         Args
  deriving (Eq, Show)

instance FromJSON FixtureFilter where
  parseJSON array@(Array args) = do
    command <- sequence $ parseJSON <$> args
    if Vector.null command
      then typeMismatch "filter" array
      else return $
           CommandFixtureFilter
             (Executable (makePath (Vector.head command)))
             (Args (Vector.toList (Vector.tail command)))
  parseJSON value = do
    contents <- parseContents id value
    return $
      case contents of
        Inline script -> InlineFixtureFilter script
        FileLocation path -> CommandFixtureFilter (Executable path) (Args [])

data Filtered a
  = Unfiltered a
  | Filtered a
             FixtureFilter
  deriving (Eq, Show)

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
