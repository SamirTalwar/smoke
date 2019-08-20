module Test.Smoke.Filters
  ( applyFilters
  , applyFiltersFromFixture
  , applyFiltersFromFixtures
  ) where

import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import Test.Smoke.Executable
import Test.Smoke.Types

type Filtering = ExceptT SmokeFilterError IO

applyFilters :: FixtureType a => Filtered a -> Filtering a
applyFilters (Unfiltered value) = return value
applyFilters (Filtered unfilteredValue (FixtureFilter executable args)) =
  runFilter executable args unfilteredValue

applyFiltersFromFixture :: FixtureType a => Fixture a -> a -> Filtering a
applyFiltersFromFixture (Fixture _ Nothing) value = return value
applyFiltersFromFixture (Fixture _ (Just fixtureFilter)) value =
  applyFilters (Filtered value fixtureFilter)

applyFiltersFromFixtures ::
     FixtureType a => Fixtures a -> a -> Filtering (Vector a)
applyFiltersFromFixtures (Fixtures fixtures) value =
  Vector.mapM (`applyFiltersFromFixture` value) fixtures

runFilter :: FixtureType a => Executable -> Args -> a -> Filtering a
runFilter executable args value = do
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (handleExecutionError executable) $
    ExceptT $
    tryIOError $
    runExecutable executable args (StdIn (serializeFixture value)) Nothing
  case exitCode of
    ExitSuccess -> return $ deserializeFixture processStdOut
    ExitFailure code ->
      throwE $
      ExecutionFailed
        executable
        (Status code)
        (StdOut processStdOut)
        (StdErr processStdErr)

handleExecutionError :: Executable -> IOError -> SmokeFilterError
handleExecutionError executable e =
  if isPermissionError e
    then NonExecutableFilter executable
    else CouldNotExecuteFilter executable e
