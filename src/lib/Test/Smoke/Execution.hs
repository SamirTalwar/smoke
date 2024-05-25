module Test.Smoke.Execution
  ( runTest,
  )
where

import Codec.Archive.Tar qualified as Tar
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import System.Directory
  ( doesDirectoryExist,
    removeDirectoryRecursive,
  )
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Error (tryIOError)
import System.IO.Temp (withSystemTempFile)
import Test.Smoke.Executable
import Test.Smoke.Paths
import Test.Smoke.Types

type Execution = ExceptT SmokeExecutionError IO

runTest :: Path Resolved Dir -> TestPlan -> IO ExecutionResult
runTest location testPlan =
  if testIgnored (planTest testPlan)
    then pure ExecutionIgnored
    else either ExecutionFailed ExecutionSucceeded <$> runExceptT (executeTest location testPlan)

executeTest :: Path Resolved Dir -> TestPlan -> Execution ActualOutputs
executeTest location (TestPlan _ workingDirectory _ executable args env processStdIn _ _ _ files revert) = do
  let workingDirectoryFilePath =
        toFilePath $ unWorkingDirectory workingDirectory
  workingDirectoryExists <- liftIO $ doesDirectoryExist workingDirectoryFilePath
  unless workingDirectoryExists $
    throwE $
      NonExistentWorkingDirectory workingDirectory
  revertingDirectories revert $ do
    (exitCode, processStdOut, processStdErr) <-
      tryIO (CouldNotExecuteCommand executable) $
        runExecutable executable args processStdIn env (Just workingDirectory)
    actualFiles <- Map.fromList <$> mapM (liftIO . readTestFile) (Map.keys files)
    pure $ ActualOutputs (convertExitCode exitCode) (StdOut processStdOut) (StdErr processStdErr) actualFiles
  where
    readTestFile :: Path Relative File -> IO (Path Resolved File, ActualFile)
    readTestFile path = do
      let absolutePath = location </> path
      contents <-
        either
          (ActualFileError . CouldNotReadFile path)
          (ActualFileContents . TestFileContents)
          <$> tryIOError (readFromPath absolutePath)
      pure (absolutePath, contents)

revertingDirectories :: Vector (Path Resolved Dir) -> Execution a -> Execution a
revertingDirectories paths execution =
  Vector.foldr revertingDirectory execution paths

revertingDirectory :: Path Resolved Dir -> Execution a -> Execution a
revertingDirectory path execution = do
  let filePath = toFilePath path
  withSystemTempFile "smoke-revert.tar" $ \tarFile handle -> do
    tryIO (CouldNotStoreDirectory path) $ do
      hClose handle
      Tar.create tarFile filePath ["."]
    result <- execution
    tryIO (CouldNotRevertDirectory path) $ do
      removeDirectoryRecursive filePath
      createDirectory path
      Tar.extract filePath tarFile
    pure result

tryIO :: (IOError -> SmokeExecutionError) -> IO a -> Execution a
tryIO handleIOError = withExceptT handleIOError . ExceptT . tryIOError

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value
