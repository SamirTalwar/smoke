module Test.Smoke.Types.Filters where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Catch.Pure (runCatchT)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as Vector
import Path
import Test.Smoke.Paths
import Test.Smoke.Types.Base

newtype FixtureFilter =
  FixtureFilter Executable
  deriving (Eq, Show)

instance FromJSON FixtureFilter where
  parseJSON array@(Array args) = do
    command <- sequence $ parseJSON <$> args
    if Vector.null command
      then typeMismatch "filter" array
      else do
        eitherExecutable <- runCatchT (parseAbsOrRelFile (Vector.head command))
        executable <- either (fail . show) return eitherExecutable
        return $
          FixtureFilter
            (ExecutableProgram executable (Args (Vector.tail command)))
  parseJSON (String script) = do
    eitherSh <- runCatchT getSh
    sh <- either (fail . show) return eitherSh
    return $ FixtureFilter (ExecutableScript (Shell sh mempty) (Script script))
  parseJSON invalid = typeMismatch "filter" invalid

data Filtered a
  = Unfiltered a
  | Filtered a FixtureFilter
  deriving (Eq, Show)

getSh :: MonadThrow m => m (Path Rel File)
getSh = parseAbsOrRelFile "sh"

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
