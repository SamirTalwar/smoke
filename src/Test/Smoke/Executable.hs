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
convertCommandToExecutable (CommandArgs command)
  | Vector.null command = fail "No command."
  | otherwise = do
    let executableName = Vector.head command
    executablePath <- parseAbsOrRelFile executableName
    let commandArgs = Vector.tail command
    return $ ExecutableProgram executablePath (Args commandArgs)
convertCommandToExecutable (CommandScript shell script) =
  return $ ExecutableScript shell script
