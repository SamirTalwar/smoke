module Test.Smoke.Execution
  ( runTest,
  )
where

import qualified Codec.Archive.Tar as Tar
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Data.Default
import Data.Map.Strict (Map, (!))
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
import Test.Smoke.Errors
import Test.Smoke.Executable
import Test.Smoke.Filters
import Test.Smoke.Paths
import Test.Smoke.Types

type Execution = ExceptT SmokeExecutionError IO

type ActualOutputs = (Status, StdOut, StdErr, ActualFiles)

type ActualFiles = Map (ResolvedPath File) TestFileContents

runTest :: ResolvedPath Dir -> TestPlan -> IO TestResult
runTest location testPlan =
  TestResult (planTest testPlan)
    <$> if testIgnored (planTest testPlan)
      then return TestIgnored
      else do
        testOutcome <-
          runExceptT $ do
            actualOutputs <- executeTest location testPlan
            processOutput location testPlan actualOutputs
        return $ handleError (TestError . ExecutionError) testOutcome

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
    return
      ( convertExitCode exitCode,
        StdOut processStdOut,
        StdErr processStdErr,
        actualFiles
      )
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

processOutput ::
  ResolvedPath Dir -> TestPlan -> ActualOutputs -> Execution TestOutcome
processOutput location testPlan@(TestPlan test _ fallbackShell _ _ _ expectedStatus expectedStdOuts expectedStdErrs expectedFiles _) (actualStatus, actualStdOut, actualStdErr, actualFiles) = do
  filteredStatus <-
    withExceptT ExecutionFilterError $
      applyFiltersFromFixture fallbackShell (testStatus test) actualStatus
  filteredStdOut <-
    withExceptT ExecutionFilterError $
      ifEmpty actualStdOut
        <$> applyFiltersFromFixtures fallbackShell (testStdOut test) actualStdOut
  filteredStdErr <-
    withExceptT ExecutionFilterError $
      ifEmpty actualStdErr
        <$> applyFiltersFromFixtures fallbackShell (testStdErr test) actualStdErr
  let statusResult = result $ Vector.singleton (expectedStatus, filteredStatus)
  let stdOutResult =
        result $ Vector.zip (defaultIfEmpty expectedStdOuts) filteredStdOut
  let stdErrResult =
        result $ Vector.zip (defaultIfEmpty expectedStdErrs) filteredStdErr
  fileResults <-
    Map.traverseWithKey
      ( \relativePath contents ->
          result . Vector.zip contents
            <$> withExceptT
              ExecutionFilterError
              ( applyFiltersFromFixtures
                  fallbackShell
                  (testFiles test ! relativePath)
                  (actualFiles ! (location </> relativePath))
              )
      )
      expectedFiles
  return $
    if statusResult == PartSuccess
      && stdOutResult == PartSuccess
      && stdErrResult == PartSuccess
      && all (== PartSuccess) (Map.elems fileResults)
      then TestSuccess
      else
        TestFailure
          testPlan
          statusResult
          stdOutResult
          stdErrResult
          fileResults
  where
    result :: Eq a => Vector (a, a) -> PartResult a
    result comparison =
      if Vector.any (uncurry (==)) comparison
        then PartSuccess
        else PartFailure comparison

tryIO :: (IOError -> SmokeExecutionError) -> IO a -> Execution a
tryIO handleIOError = withExceptT handleIOError . ExceptT . tryIOError

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value

ifEmpty :: a -> Vector a -> Vector a
ifEmpty x xs
  | Vector.null xs = Vector.singleton x
  | otherwise = xs

defaultIfEmpty :: Default a => Vector a -> Vector a
defaultIfEmpty = ifEmpty def
