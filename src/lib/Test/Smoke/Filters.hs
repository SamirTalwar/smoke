module Test.Smoke.Filters where

import Control.Monad.Trans.Except (ExceptT (..), throwE, withExceptT)
import System.Exit (ExitCode (..))
import System.IO.Error (tryIOError)
import Test.Smoke.Executable
import Test.Smoke.Types

type Filtering = ExceptT SmokeFilterError IO

applyFilters :: (FromFixture a, ToFixture a) => Maybe Shell -> Filter -> a -> Filtering a
applyFilters fallbackShell (Filter command) value = do
  executable <-
    withExceptT FilterPathError $
      convertCommandToExecutable fallbackShell command
  (exitCode, processStdOut, processStdErr) <-
    withExceptT (CouldNotExecuteFilter executable) $
      ExceptT $
        tryIOError $
          runExecutable executable mempty (StdIn (serializeFixture value)) Nothing Nothing
  case exitCode of
    ExitSuccess -> pure $ deserializeFixture processStdOut
    ExitFailure code ->
      throwE $
        FilterExecutionFailed
          executable
          (Status code)
          (StdOut processStdOut)
          (StdErr processStdErr)
