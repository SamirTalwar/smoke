module Test.Smoke.Types.Filters where

import Control.Monad.Catch.Pure (runCatchT)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types.Base

data FixtureFilter
  = InlineFixtureFilter Text
  | CommandFixtureFilter Executable Args
  deriving (Eq, Show)

instance FromJSON FixtureFilter where
  parseJSON array@(Array args) = do
    command <- sequence $ parseJSON <$> args
    if Vector.null command
      then typeMismatch "filter" array
      else do
        eitherExecutable <- runCatchT (parseAbsOrRelFile (Vector.head command))
        case eitherExecutable of
          Left exception -> fail $ show exception
          Right executable ->
            return $
            CommandFixtureFilter
              (Executable executable)
              (Args (Vector.toList (Vector.tail command)))
  parseJSON (String script) = return $ InlineFixtureFilter script
  parseJSON invalid = typeMismatch "filter" invalid

data Filtered a
  = Unfiltered a
  | Filtered a FixtureFilter
  deriving (Eq, Show)

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
