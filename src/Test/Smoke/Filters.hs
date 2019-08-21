module Test.Smoke.Filters
  ( applyFilters
  , applyFiltersFromFixture
  , applyFiltersFromFixtures
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import Test.Smoke.Executable
import Test.Smoke.Types

type Filtering = ExceptT SmokeFilterError IO

applyFilters :: FixtureType a => Shell -> Filtered a -> Filtering a
applyFilters _ (Unfiltered value) = return value
applyFilters fallbackShell (Filtered unfilteredValue command) =
  runFilter fallbackShell command unfilteredValue

applyFiltersFromFixture ::
     FixtureType a => Shell -> Fixture a -> a -> Filtering a
applyFiltersFromFixture _ (Fixture _ Nothing) value = return value
applyFiltersFromFixture fallbackShell (Fixture _ (Just fixtureFilter)) value =
  applyFilters fallbackShell (Filtered value fixtureFilter)

applyFiltersFromFixtures ::
     FixtureType a => Shell -> Fixtures a -> a -> Filtering (Vector a)
applyFiltersFromFixtures fallbackShell (Fixtures fixtures) value =
  Vector.mapM
    (\fixture -> applyFiltersFromFixture fallbackShell fixture value)
    fixtures

runFilter :: FixtureType a => Shell -> Command -> a -> Filtering a
runFilter fallbackShell command value = do
  executable <- liftIO $ convertCommandToExecutable fallbackShell command
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (handleExecutionError executable) $
    ExceptT $
    tryIOError $
    runExecutable executable mempty (StdIn (serializeFixture value)) Nothing
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
