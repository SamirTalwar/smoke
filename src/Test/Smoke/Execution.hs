{-# LANGUAGE LambdaCase #-}

module Test.Smoke.Execution
  ( runTests
  ) where

import qualified Codec.Archive.Tar as Tar
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Default
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Path
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , removeDirectoryRecursive
  )
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.IO.Error (isPermissionError, tryIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process.ListLike (cwd, proc)
import System.Process.Text (readCreateProcessWithExitCode)
import Test.Smoke.Errors
import Test.Smoke.Filters
import Test.Smoke.Maps
import Test.Smoke.Paths
import Test.Smoke.Types

type Execution = ExceptT SmokeExecutionError IO

type ActualOutputs = (Status, StdOut, StdErr, ActualFiles)

type ActualFiles = Map (Path Abs File) TestFileContents

runTests :: Plan -> IO Results
runTests (Plan suites) =
  forM suites $ \case
    SuitePlanError suiteName errorMessage ->
      return $ SuiteResultError suiteName errorMessage
    SuitePlan suiteName location testPlans -> do
      testResults <-
        forM testPlans $ \case
          Left (TestPlanError test errorMessage) ->
            return $ TestResult test $ TestError $ PlanningError errorMessage
          Right testPlan -> runTest location testPlan
      return $ SuiteResult suiteName location testResults

runTest :: Path Abs Dir -> TestPlan -> IO TestResult
runTest location testPlan =
  handleError (TestResult (planTest testPlan) . TestError . ExecutionError) <$>
  runExceptT (processOutput location testPlan =<< executeTest location testPlan)

executeTest :: Path Abs Dir -> TestPlan -> Execution ActualOutputs
executeTest location (TestPlan _ workingDirectory executable (Args args) (StdIn processStdIn) _ _ _ files revert) = do
  let workingDirectoryFilePath =
        toFilePath $ unWorkingDirectory workingDirectory
  workingDirectoryExists <- liftIO $ doesDirectoryExist workingDirectoryFilePath
  unless workingDirectoryExists $
    throwE $ NonExistentWorkingDirectory workingDirectory
  let executableName = toFilePath $ unExecutable executable
  revertingDirectories revert $ do
    (exitCode, processStdOut, processStdErr) <-
      tryIO (handleExecutionError executable) $
      readCreateProcessWithExitCode
        ((proc executableName args) {cwd = Just workingDirectoryFilePath})
        processStdIn
    actualFiles <-
      Map.map TestFileContents . Map.fromList <$>
      mapM readTestFile (Map.keys files)
    return
      ( convertExitCode exitCode
      , StdOut processStdOut
      , StdErr processStdErr
      , actualFiles)
  where
    readTestFile :: Path Rel File -> Execution (Path Abs File, Text)
    readTestFile path = do
      let absolutePath = location </> path
      contents <-
        tryIO (CouldNotReadFile path . show) $ readFromPath absolutePath
      return (absolutePath, contents)

revertingDirectories :: Vector (Path Rel Dir) -> Execution a -> Execution a
revertingDirectories paths execution
  | Vector.null paths = execution
  | otherwise = do
    let path = Vector.head paths
    let filePath = toFilePath path
    withSystemTempFile "smoke-revert.tar" $ \tarFile handle -> do
      tryIO (CouldNotStoreDirectory path . show) $ do
        hClose handle
        Tar.create tarFile filePath ["."]
      result <- execution
      tryIO (CouldNotRevertDirectory path . show) $ do
        removeDirectoryRecursive filePath
        createDirectory filePath
        Tar.extract filePath tarFile
      return result

processOutput ::
     Path Abs Dir -> TestPlan -> ActualOutputs -> Execution TestResult
processOutput location testPlan@(TestPlan test _ _ _ _ expectedStatus expectedStdOuts expectedStdErrs expectedFiles _) (actualStatus, actualStdOut, actualStdErr, actualFiles) = do
  filteredStatus <-
    withExceptT ExecutionFilterError $
    applyFiltersFromFixture (testStatus test) actualStatus
  filteredStdOut <-
    withExceptT ExecutionFilterError $
    ifEmpty actualStdOut <$>
    applyFiltersFromFixtures (testStdOut test) actualStdOut
  filteredStdErr <-
    withExceptT ExecutionFilterError $
    ifEmpty actualStdErr <$>
    applyFiltersFromFixtures (testStdErr test) actualStdErr
  let statusResult = result $ Vector.singleton (expectedStatus, filteredStatus)
  let stdOutResult =
        result $ Vector.zip (defaultIfEmpty expectedStdOuts) filteredStdOut
  let stdErrResult =
        result $ Vector.zip (defaultIfEmpty expectedStdErrs) filteredStdErr
  fileResults <-
    mapWithKeyM
      (\relativePath contents ->
         result . Vector.zip contents <$>
         withExceptT
           ExecutionFilterError
           (applyFiltersFromFixtures
              (testFiles test ! relativePath)
              (actualFiles ! (location </> relativePath))))
      expectedFiles
  return $
    TestResult test $
    if statusResult == PartSuccess &&
       stdOutResult == PartSuccess &&
       stdErrResult == PartSuccess &&
       all (== PartSuccess) (Map.elems fileResults)
      then TestSuccess
      else TestFailure
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

handleExecutionError :: Executable -> IOError -> SmokeExecutionError
handleExecutionError executable e =
  if isPermissionError e
    then NonExecutableCommand executable
    else CouldNotExecuteCommand executable (show e)

convertExitCode :: ExitCode -> Status
convertExitCode ExitSuccess = Status 0
convertExitCode (ExitFailure value) = Status value

ifEmpty :: a -> Vector a -> Vector a
ifEmpty x xs
  | Vector.null xs = Vector.singleton x
  | otherwise = xs

defaultIfEmpty :: Default a => Vector a -> Vector a
defaultIfEmpty = ifEmpty def
