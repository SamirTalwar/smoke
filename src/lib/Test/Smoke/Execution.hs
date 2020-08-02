module Test.Smoke.Execution
  ( runTest,
  )
where

import qualified Codec.Archive.Tar as Tar
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
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

runTest :: ResolvedPath Dir -> TestPlan -> IO ExecutionResult
runTest location testPlan =
  if testIgnored (planTest testPlan)
    then return ExecutionIgnored
    else either ExecutionFailed ExecutionSucceeded <$> runExceptT (executeTest location testPlan)

executeTest :: ResolvedPath Dir -> TestPlan -> Execution ActualOutputs
executeTest location (TestPlan _ workingDirectory _ executable args processStdIn _ _ _ files revert) = do
  let workingDirectoryFilePath =
        toFilePath $ unWorkingDirectory workingDirectory
  workingDirectoryExists <- liftIO $ doesDirectoryExist workingDirectoryFilePath
  unless workingDirectoryExists $
    throwE $
      NonExistentWorkingDirectory workingDirectory
  revertingDirectories revert $ do
    (exitCode, processStdOut, processStdErr) <-
      tryIO (CouldNotExecuteCommand executable) $
        runExecutable executable args processStdIn (Just workingDirectory)
    actualFiles <-
      Map.map TestFileContents . Map.fromList
        <$> mapM readTestFile (Map.keys files)
    return $ ActualOutputs (convertExitCode exitCode) (StdOut processStdOut) (StdErr processStdErr) actualFiles
  where
    readTestFile :: RelativePath File -> Execution (ResolvedPath File, Text)
    readTestFile path = do
      let absolutePath = location </> path
      contents <- tryIO (CouldNotReadFile path) $ readFromPath absolutePath
      return (absolutePath, contents)

revertingDirectories :: Vector (ResolvedPath Dir) -> Execution a -> Execution a
revertingDirectories paths execution =
  Vector.foldr revertingDirectory execution paths

revertingDirectory :: ResolvedPath Dir -> Execution a -> Execution a
revertingDirectory path execution = do
  let filePath = toFilePath path
  withSystemTempFile "smoke-revert.tar" $ \tarFile handle -> do
    tryIO (CouldNotStoreDirectory path) $ do
      hClose handle
      Tar.create tarFile filePath ["."]
    result <- execution
    tryIO (CouldNotRevertDirectory path) $ do
      removeDirectoryRecursive filePath
      createDirectory filePath
      Tar.extract filePath tarFile
    return result

tryIO :: (IOError -> SmokeExecutionError) -> IO a -> Execution a
tryIO handleIOError = withExceptT handleIOError . ExceptT . tryIOError

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value
