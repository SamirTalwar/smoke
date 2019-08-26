{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Executable where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified System.Directory as Directory
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess(..), proc)
import System.Process.Text (readCreateProcessWithExitCode)
import Test.Smoke.Paths
import Test.Smoke.Types

defaultShell :: ExceptT SmokeExecutableError IO Shell
defaultShell = do
  sh <- findExecutable $ parseFile "sh"
  return $ Shell sh mempty

defaultShellExecute :: Vector String
defaultShellExecute = Vector.fromList ["sh", "-c"]

shellFromCommandLine :: CommandLine -> ExceptT SmokeExecutableError IO Shell
shellFromCommandLine (CommandLine shellName shellArgs) = do
  shellCommand <- findExecutable shellName
  return $ Shell shellCommand shellArgs

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

convertCommandToExecutable ::
     Maybe Shell -> Command -> ExceptT SmokeExecutableError IO Executable
convertCommandToExecutable _ (CommandArgs (CommandLine executableName commandArgs)) = do
  executablePath <- findExecutable executableName
  return $ ExecutableProgram executablePath commandArgs
convertCommandToExecutable Nothing (CommandScript Nothing script) = do
  shell <- defaultShell
  return $ ExecutableScript shell script
convertCommandToExecutable (Just shell) (CommandScript Nothing script) =
  return $ ExecutableScript shell script
convertCommandToExecutable _ (CommandScript (Just commandLine) script) = do
  shell <- shellFromCommandLine commandLine
  return $ ExecutableScript shell script

findExecutable ::
     RelativePath File -> ExceptT SmokeExecutableError IO (ResolvedPath File)
findExecutable path = do
  exists <- liftIO $ Directory.doesFileExist (toFilePath path)
  if exists
    then do
      permissions <- liftIO $ Directory.getPermissions (toFilePath path)
      unless (Directory.executable permissions) $
        throwE $ FileIsNotExecutable path
      liftIO $ resolve path
    else do
      executable <- liftIO $ Directory.findExecutable (toFilePath path)
      maybe
        (throwE $ CouldNotFindExecutable path)
        (liftIO . resolve . parseFile)
        executable
