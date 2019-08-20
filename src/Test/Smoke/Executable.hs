module Test.Smoke.Executable where

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Path
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess(..), proc)
import System.Process.Text (readCreateProcessWithExitCode)
import Test.Smoke.Types.Base

runExecutable ::
     Executable
  -> Args
  -> StdIn
  -> Maybe WorkingDirectory
  -> IO (ExitCode, Text, Text)
runExecutable (ExecutableProgram executablePath) (Args args) (StdIn stdIn) workingDirectory =
  readCreateProcessWithExitCode
    ((proc (toFilePath executablePath) args)
       {cwd = toFilePath . unWorkingDirectory <$> workingDirectory})
    stdIn
runExecutable (ExecutableScript (Shell shell) script) (Args args) (StdIn stdIn) workingDirectory =
  withSystemTempFile "smoke.sh" $ \scriptPath scriptHandle -> do
    Text.IO.hPutStr scriptHandle script
    hClose scriptHandle
    readCreateProcessWithExitCode
      ((proc (head shell) (tail shell ++ [scriptPath] ++ args))
         {cwd = toFilePath . unWorkingDirectory <$> workingDirectory})
      stdIn
