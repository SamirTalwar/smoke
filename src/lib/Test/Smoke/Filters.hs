module Test.Smoke.Filters
  ( applyFilters,
    applyFiltersFromFixture,
    applyFiltersFromFixtures,
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

applyFilters :: FixtureType a => Maybe Shell -> Filtered a -> Filtering a
applyFilters _ (Unfiltered value) = return value
applyFilters fallbackShell (Filtered unfilteredValue command) =
  runFilter fallbackShell command unfilteredValue

applyFiltersFromFixture ::
  FixtureType a => Maybe Shell -> Fixture a -> a -> Filtering a
applyFiltersFromFixture _ (Fixture _ Nothing) value = return value
applyFiltersFromFixture fallbackShell (Fixture _ (Just fixtureFilter)) value =
  applyFilters fallbackShell (Filtered value fixtureFilter)

applyFiltersFromFixtures ::
  FixtureType a => Maybe Shell -> Fixtures a -> a -> Filtering (Vector a)
applyFiltersFromFixtures fallbackShell (Fixtures fixtures) value =
  Vector.mapM
    (\fixture -> applyFiltersFromFixture fallbackShell fixture value)
    fixtures

runFilter :: FixtureType a => Maybe Shell -> Command -> a -> Filtering a
runFilter fallbackShell command value = do
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
