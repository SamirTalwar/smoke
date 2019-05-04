module Test.Smoke.Paths where

import Control.Monad.Catch (MonadThrow)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Path
import System.Directory (doesPathExist, getCurrentDirectory)
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob

-- Parse
parseAbsOrRelDir :: MonadThrow m => FilePath -> m (Path Rel Dir)
parseAbsOrRelDir = parseRelDir . normalize

parseAbsOrRelFile :: MonadThrow m => FilePath -> m (Path Rel File)
parseAbsOrRelFile = parseRelFile . normalize

(<//>) :: MonadThrow m => Path Abs Dir -> FilePath -> m (Path Abs Dir)
a <//> b = parseAbsDir $ normalize $ toFilePath a FilePath.</> b

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
pathExists path = doesPathExist $ toFilePath path

-- Search
getCurrentWorkingDirectory :: IO (Path Abs Dir)
getCurrentWorkingDirectory = parseAbsDir =<< getCurrentDirectory

resolvePath :: Path Rel t -> IO (Path Abs t)
resolvePath path = do
  currentWorkingDirectory <- getCurrentWorkingDirectory
  return $ currentWorkingDirectory </> path

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
