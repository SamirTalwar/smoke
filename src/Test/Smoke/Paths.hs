module Test.Smoke.Paths where

import Control.Monad (unless)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Path
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob
import Test.Smoke.Errors
import Test.Smoke.Types.Errors

-- Parse
(<//>) :: MonadFail m => Path Abs Dir -> FilePath -> m (Path Abs Dir)
a <//> b = catchAndFail . parseAbsDir . normalize $ toFilePath a FilePath.</> b

normalize :: FilePath -> FilePath
normalize =
  FilePath.joinPath .
  interpretParentAccess . FilePath.splitPath . FilePath.normalise
  where
    interpretParentAccess [] = []
    interpretParentAccess [x] = [x]
    interpretParentAccess (x:y:rest) =
      if FilePath.normalise y == ".."
        then interpretParentAccess rest
        else x : interpretParentAccess (y : rest)

-- Query
pathExists :: Path b t -> IO Bool
pathExists path = Directory.doesPathExist $ toFilePath path

findExecutable :: FilePath -> ExceptT SmokeExecutableError IO (Path Abs File)
findExecutable filePath = do
  exists <- liftIO $ Directory.doesFileExist filePath
  if exists
    then do
      permissions <- liftIO $ Directory.getPermissions filePath
      unless (Directory.executable permissions) $
        throwE $ FileIsNotExecutable filePath
      currentWorkingDirectory <- liftIO Directory.getCurrentDirectory
      parseAbsFile $ currentWorkingDirectory FilePath.</> filePath
    else do
      executable <- liftIO $ Directory.findExecutable filePath
      maybe
        (throwE $ CouldNotFindExecutable filePath)
        (liftIO . parseAbsFile)
        executable

-- Search
getCurrentWorkingDirectory :: IO (Path Abs Dir)
getCurrentWorkingDirectory = parseAbsDir =<< Directory.getCurrentDirectory

resolvePath :: Path Rel t -> IO (Path Abs t)
resolvePath path = do
  currentWorkingDirectory <- getCurrentWorkingDirectory
  return $ currentWorkingDirectory </> path

resolveDir :: FilePath -> IO (Path Abs Dir)
resolveDir = resolveWith parseAbsDir

resolveFile :: FilePath -> IO (Path Abs File)
resolveFile = resolveWith parseAbsFile

resolveWith :: (FilePath -> IO (Path Abs t)) -> FilePath -> IO (Path Abs t)
resolveWith parse filePath = do
  currentWorkingDirectory <- Directory.getCurrentDirectory
  parse $ currentWorkingDirectory FilePath.</> filePath

findFilesInPath :: Glob.Pattern -> Path Rel Dir -> IO [Path Rel File]
findFilesInPath filePattern path =
  mapM parseRelFile =<< Glob.globDir1 filePattern (toFilePath path)

yamlFiles :: Glob.Pattern
yamlFiles = Glob.compile "*.yaml"

-- I/O
readFromPath :: Path Abs File -> IO Text
readFromPath = TextIO.readFile . toFilePath

writeToPath :: Path Abs File -> Text -> IO ()
writeToPath = TextIO.writeFile . toFilePath
