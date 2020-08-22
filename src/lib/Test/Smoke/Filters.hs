module Test.Smoke.Filters
  ( applyFilters,
    applyFiltersFromTestOutput,
    applyFiltersFromTestOutputs,
  )
where

import Control.Monad.Trans.Except (ExceptT (..), throwE, withExceptT)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Exit (ExitCode (..))
import System.IO.Error (tryIOError)
import Test.Smoke.Executable
import Test.Smoke.Types

type Filtering = ExceptT SmokeFilterError IO

applyFilters :: FixtureType a => Maybe Shell -> Filter -> a -> Filtering a
applyFilters fallbackShell (Filter command) value = do
  executable <-
    withExceptT FilterPathError $
      convertCommandToExecutable fallbackShell command
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (CouldNotExecuteFilter executable) $
      ExceptT $
        tryIOError $
          runExecutable executable mempty (StdIn (serializeFixture value)) Nothing
  case exitCode of
    ExitSuccess -> return $ deserializeFixture processStdOut
    ExitFailure code ->
      throwE $
        FilterExecutionFailed
          executable
          (Status code)
          (StdOut processStdOut)
          (StdErr processStdErr)

applyFiltersFromTestOutput ::
  FixtureType a => Maybe Shell -> TestOutput a -> a -> Filtering a
applyFiltersFromTestOutput _ (TestOutput _ Nothing _) value =
  return value
applyFiltersFromTestOutput fallbackShell (TestOutput _ (Just fixtureFilter) _) value =
  applyFilters fallbackShell fixtureFilter value

applyFiltersFromTestOutputs ::
  FixtureType a => Maybe Shell -> Vector (TestOutput a) -> a -> Filtering (Vector a)
applyFiltersFromTestOutputs fallbackShell assertables value =
  Vector.mapM
    (\assertable -> applyFiltersFromTestOutput fallbackShell assertable value)
    assertables
