module Test.Smoke.Filters
  ( applyFilters
  ) where

import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.IO.Error (isPermissionError, tryIOError)
import System.Process.Text (readProcessWithExitCode)
import Test.Smoke.Types

type Filtering = ExceptT TestFilterErrorMessage IO

applyFilters :: FixtureType a => Filtered a -> Filtering a
applyFilters (Unfiltered value) = return value
applyFilters (Filtered unfilteredValue (FixtureFilter (Inline script))) =
  runScript
    (Executable (makePath "sh"))
    (Args ["-c", Text.unpack script])
    unfilteredValue
applyFilters (Filtered unfilteredValue (FixtureFilter (FileLocation scriptPath))) =
  runScript (Executable scriptPath) (Args []) unfilteredValue

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

handleExecutionError :: Executable -> IOError -> TestFilterErrorMessage
handleExecutionError executable e =
  if isPermissionError e
    then NonExecutableFilter executable
    else CouldNotExecuteFilter executable (show e)
