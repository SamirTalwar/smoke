module Test.Smoke.Executable where

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import Path
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess(..), proc)
import System.Process.Text (readCreateProcessWithExitCode)
import Test.Smoke.Paths
import Test.Smoke.Shell
import Test.Smoke.Types

runExecutable ::
     Executable
  -> Args
  -> StdIn
  -> Maybe WorkingDirectory
  -> IO (ExitCode, Text, Text)
runExecutable (ExecutableProgram executablePath executableArgs) args (StdIn stdIn) workingDirectory =
  readCreateProcessWithExitCode
    ((proc
        (toFilePath executablePath)
        (Vector.toList (unArgs (executableArgs <> args))))
       {cwd = toFilePath . unWorkingDirectory <$> workingDirectory})
    stdIn
runExecutable (ExecutableScript (Shell shellPath shellArgs) (Script script)) args stdIn workingDirectory =
  withSystemTempFile "smoke.sh" $ \scriptPath scriptHandle -> do
    Text.IO.hPutStr scriptHandle script
    hClose scriptHandle
    let executableArgs = shellArgs <> Args (Vector.singleton scriptPath)
    runExecutable
      (ExecutableProgram shellPath executableArgs)
      args
      stdIn
      workingDirectory

convertCommandToExecutable :: Command -> IO Executable
convertCommandToExecutable (CommandArgs (CommandLine executableName commandArgs)) = do
  executablePath <- parseAbsOrRelFile executableName
  return $ ExecutableProgram executablePath commandArgs
convertCommandToExecutable (CommandScript Nothing script) = do
  shell <- defaultShell
  return $ ExecutableScript shell script
convertCommandToExecutable (CommandScript (Just (CommandLine shellName shellArgs)) script) = do
  shellCommand <- parseAbsOrRelFile shellName
  return $ ExecutableScript (Shell shellCommand shellArgs) script
