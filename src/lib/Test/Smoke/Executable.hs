module Test.Smoke.Executable where

import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Data.Vector qualified as Vector
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
  Maybe EnvVars ->
  Maybe WorkingDirectory ->
  IO (ExitCode, Text, Text)
runExecutable (ExecutableProgram executablePath executableArgs) args (StdIn stdIn) env workingDirectory =
  readCreateProcessWithExitCode
    ( ( proc
          (toFilePath executablePath)
          (Vector.toList (unArgs (executableArgs <> args)))
      )
        { cwd = toFilePath . unWorkingDirectory <$> workingDirectory,
          env = fmap Map.toList (unEnvVars <$> env)
        }
    )
    stdIn
runExecutable (ExecutableScript (Shell shellPath shellArgs) (Script script)) args stdIn env workingDirectory =
  withSystemTempFile defaultShellScriptName $ \scriptPath scriptHandle -> do
    Text.IO.hPutStr scriptHandle script
    hClose scriptHandle
    let executableArgs = shellArgs <> Args (Vector.singleton scriptPath)
    runExecutable
      (ExecutableProgram shellPath executableArgs)
      args
      stdIn
      env
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
