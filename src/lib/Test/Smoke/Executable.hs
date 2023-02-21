module Test.Smoke.Executable where

import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess (..), proc)
import System.Process.Text (readCreateProcessWithExitCode)
import Test.Smoke.Paths
import Test.Smoke.Shell
import Test.Smoke.Types

runExecutable ::
  Executable ->
  Args ->
  StdIn ->
  Maybe WorkingDirectory ->
  IO (ExitCode, Text, Text)
runExecutable (ExecutableProgram executablePath executableArgs) args (StdIn stdIn) workingDirectory =
  readCreateProcessWithExitCode
    ( ( proc
          (toFilePath executablePath)
          (Vector.toList (unArgs (executableArgs <> args)))
      )
        { cwd = toFilePath . unWorkingDirectory <$> workingDirectory
        }
    )
    stdIn
runExecutable (ExecutableScript (Shell shellPath shellArgs) (Script script)) args stdIn workingDirectory =
  withSystemTempFile defaultShellScriptName $ \scriptPath scriptHandle -> do
    Text.IO.hPutStr scriptHandle script
    hClose scriptHandle
    let executableArgs = shellArgs <> Args (Vector.singleton scriptPath)
    runExecutable
      (ExecutableProgram shellPath executableArgs)
      args
      stdIn
      workingDirectory

convertCommandToExecutable ::
  Maybe Shell -> Command -> ExceptT PathError IO Executable
convertCommandToExecutable _ (CommandArgs (CommandLine executableName commandArgs)) = do
  executablePath <- findExecutable executableName
  pure $ ExecutableProgram executablePath commandArgs
convertCommandToExecutable Nothing (CommandScript Nothing script) = do
  shell <- defaultShell
  pure $ ExecutableScript shell script
convertCommandToExecutable (Just shell) (CommandScript Nothing script) =
  pure $ ExecutableScript shell script
convertCommandToExecutable _ (CommandScript (Just commandLine) script) = do
  shell <- shellFromCommandLine commandLine
  pure $ ExecutableScript shell script

shellFromCommandLine :: CommandLine -> ExceptT PathError IO Shell
shellFromCommandLine (CommandLine shellName shellArgs) = do
  shellCommand <- findExecutable shellName
  pure $ Shell shellCommand shellArgs
