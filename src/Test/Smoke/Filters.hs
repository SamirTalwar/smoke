module Test.Smoke.Filters
  ( applyFilters
  , applyFiltersFromFixture
  , applyFiltersFromFixtures
  ) where

import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.Types

type Filtering = ExceptT SmokeFilterError IO

applyFilters :: FixtureType a => Filtered a -> Filtering a
applyFilters (Unfiltered value) = return value
applyFilters (Filtered unfilteredValue (InlineFixtureFilter script)) =
  runScript
    (Executable (makePath "sh"))
    (Args ["-c", Text.unpack script])
    unfilteredValue
applyFilters (Filtered unfilteredValue (CommandFixtureFilter scriptExecutable scriptArgs)) =
  runScript scriptExecutable scriptArgs unfilteredValue

applyFiltersFromFixture :: FixtureType a => Fixture a -> a -> Filtering a
applyFiltersFromFixture (Fixture _ Nothing) value = return value
applyFiltersFromFixture (Fixture _ (Just fixtureFilter)) value =
  applyFilters (Filtered value fixtureFilter)

applyFiltersFromFixtures ::
     FixtureType a => Fixtures a -> a -> Filtering (Vector a)
applyFiltersFromFixtures (Fixtures fixtures) value =
  Vector.mapM (`applyFiltersFromFixture` value) fixtures

runScript :: FixtureType a => Executable -> Args -> a -> Filtering a
runScript executable (Args args) value = do
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (handleExecutionError executable) $
    ExceptT $
    tryIOError $
    readProcessWithExitCode
      (show (unExecutable executable))
      args
      (serializeFixture value)
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
    else CouldNotExecuteFilter executable (show e)
