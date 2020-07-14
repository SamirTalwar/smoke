{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Smoke.Paths
  ( File,
    Dir,
    Path,
    RelativePath,
    ResolvedPath,
    PathError (..),
    (</>),
    findExecutable,
    findFilesInPath,
    getCurrentWorkingDirectory,
    parent,
    parseDir,
    parseFile,
    readFromPath,
    resolve,
    toFilePath,
    writeToPath,
  )
where

import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob

data Dir

data File

newtype RelativePath t
  = RelativePath FilePath
  deriving (Eq, Ord, Show)

newtype ResolvedPath t
  = ResolvedPath FilePath
  deriving (Eq, Ord, Show)

class Path p t where
  fromFilePath :: FilePath -> p t
  toFilePath :: p t -> FilePath

instance Path RelativePath Dir where
  fromFilePath = RelativePath . normalizeFilePath
  toFilePath (RelativePath filePath) = filePath ++ pure FilePath.pathSeparator

instance Path RelativePath File where
  fromFilePath = RelativePath . normalizeFilePath
  toFilePath (RelativePath filePath) = filePath

instance Path ResolvedPath Dir where
  fromFilePath = ResolvedPath . normalizeFilePath
  toFilePath (ResolvedPath filePath) = filePath ++ pure FilePath.pathSeparator

instance Path ResolvedPath File where
  fromFilePath = ResolvedPath . normalizeFilePath
  toFilePath (ResolvedPath filePath) = filePath

instance FromJSON (RelativePath Dir) where
  parseJSON = withText "path" (return . parseDir . Text.unpack)

instance FromJSON (RelativePath File) where
  parseJSON = withText "path" (return . parseFile . Text.unpack)

-- Construct
parseDir :: FilePath -> RelativePath Dir
parseDir = fromFilePath

parseFile :: FilePath -> RelativePath File
parseFile = fromFilePath

normalizeFilePath :: FilePath -> FilePath
normalizeFilePath filePath =
  let (drive, path) = FilePath.splitDrive filePath
   in FilePath.joinDrive drive (normalizePath path)
  where
    normalizePath :: FilePath -> FilePath
    normalizePath =
      FilePath.normalise
        . FilePath.joinPath
        . interpretParentAccess
        . removeTrailingSeparators
        . FilePath.splitPath
        . FilePath.normalise
    removeTrailingSeparators :: [FilePath] -> [FilePath]
    removeTrailingSeparators = map removeTrailingSeparator
    removeTrailingSeparator :: FilePath -> FilePath
    removeTrailingSeparator segment
      | FilePath.isDrive segment = segment
      | otherwise = takeWhile isNotSeparator segment
    isNotSeparator :: Char -> Bool
    isNotSeparator = flip notElem FilePath.pathSeparators
    interpretParentAccess :: [FilePath] -> [FilePath]
    interpretParentAccess = reverse . interpretParentAccess' []
    interpretParentAccess' :: [FilePath] -> [FilePath] -> [FilePath]
    interpretParentAccess' before [] = before
    interpretParentAccess' (".." : before) (".." : after) =
      interpretParentAccess' (".." : ".." : before) after
    interpretParentAccess' (_ : before) (".." : after) =
      interpretParentAccess' before after
    interpretParentAccess' before (x : xs) =
      interpretParentAccess' (x : before) xs

-- Manipulate
(</>) ::
  (Path RelativePath t, Path ResolvedPath t) =>
  ResolvedPath Dir ->
  RelativePath t ->
  ResolvedPath t
ResolvedPath a </> RelativePath b = fromFilePath (a FilePath.</> b)

parent :: (Path p t, Path p Dir) => p t -> p Dir
parent = fromFilePath . FilePath.dropFileName . toFilePath

-- Resolve
resolve ::
  (Path RelativePath t, Path ResolvedPath t) =>
  RelativePath t ->
  IO (ResolvedPath t)
resolve path = do
  currentWorkingDirectory <- getCurrentWorkingDirectory
  return $ currentWorkingDirectory </> path

getCurrentWorkingDirectory :: IO (ResolvedPath Dir)
getCurrentWorkingDirectory = ResolvedPath <$> Directory.getCurrentDirectory

findExecutable :: RelativePath File -> ExceptT PathError IO (ResolvedPath File)
findExecutable path = do
  exists <- liftIO $ Directory.doesFileExist (toFilePath path)
  if exists
    then do
      permissions <- liftIO $ Directory.getPermissions (toFilePath path)
      unless (Directory.executable permissions) $
        throwE $
          FileIsNotExecutable path
      liftIO $ resolve path
    else do
      executable <- liftIO $ Directory.findExecutable (toFilePath path)
      maybe
        (throwE $ CouldNotFindExecutable path)
        (liftIO . resolve . parseFile)
        executable

-- Search
findFilesInPath ::
  (Path p Dir, Path p File) => Glob.Pattern -> p Dir -> IO [p File]
findFilesInPath filePattern path =
  map fromFilePath <$> Glob.globDir1 filePattern (toFilePath path)

-- I/O
readFromPath :: ResolvedPath File -> IO Text
readFromPath = Text.IO.readFile . toFilePath

writeToPath :: ResolvedPath File -> Text -> IO ()
writeToPath = Text.IO.writeFile . toFilePath

-- Errors
data PathError
  = CouldNotFindExecutable (RelativePath File)
  | FileIsNotExecutable (RelativePath File)
  deriving (Eq, Show)

instance Exception PathError
