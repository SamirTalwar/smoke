{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Smoke.Paths
  ( Path,
    PathFormat (..),
    PathObject (..),
    PathError (..),
    ConvertPath (..),
    (</>),
    createDirectory,
    createParent,
    findExecutable,
    findFilesInPath,
    getCurrentWorkingDirectory,
    parent,
    parseDir,
    parseFile,
    readFromPath,
    resolve,
    writeToPath,
  )
where

import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Kind
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob

data PathFormat = Relative | Resolved
  deriving (Eq, Ord, Show)

data PathObject = Dir | File
  deriving (Eq, Ord, Show)

data Path :: PathFormat -> PathObject -> Type where
  Path :: FilePath -> Path f o

deriving instance Eq (Path f o)

deriving instance Ord (Path f o)

instance Show (Path f o) where
  show (Path filePath) = filePath

fromFilePath :: FilePath -> Path f o
fromFilePath = Path . normalizeFilePath

class ConvertPath (f :: PathFormat) (o :: PathObject) where
  toFilePath :: Path f o -> FilePath

instance ConvertPath f Dir where
  toFilePath (Path filePath) = filePath ++ pure FilePath.pathSeparator

instance ConvertPath f File where
  toFilePath (Path filePath) = filePath

instance FromJSON (Path f o) where
  parseJSON = withText "path" (return . fromFilePath . Text.unpack)

-- Construct
parseDir :: FilePath -> Path Relative Dir
parseDir = fromFilePath

parseFile :: FilePath -> Path Relative File
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
(</>) :: Path Resolved Dir -> Path Relative t -> Path Resolved t
Path a </> Path b = fromFilePath (a FilePath.</> b)

parent :: Path p t -> Path p Dir
parent (Path filePath) = fromFilePath (FilePath.dropFileName filePath)

-- Resolve
resolve :: Path Relative t -> IO (Path Resolved t)
resolve path = do
  currentWorkingDirectory <- getCurrentWorkingDirectory
  return $ currentWorkingDirectory </> path

getCurrentWorkingDirectory :: IO (Path Resolved Dir)
getCurrentWorkingDirectory = Path <$> Directory.getCurrentDirectory

findExecutable :: Path Relative File -> ExceptT PathError IO (Path Resolved File)
findExecutable path@(Path filePath) = do
  exists <- liftIO $ Directory.doesFileExist filePath
  if exists
    then do
      permissions <- liftIO $ Directory.getPermissions filePath
      unless (Directory.executable permissions) $
        throwE $
          FileIsNotExecutable path
      liftIO $ resolve path
    else do
      executable <- liftIO $ Directory.findExecutable filePath
      maybe
        (throwE $ CouldNotFindExecutable path)
        (liftIO . resolve . parseFile)
        executable

-- Search
findFilesInPath :: Glob.Pattern -> Path p Dir -> IO [Path p File]
findFilesInPath filePattern (Path filePath) =
  map fromFilePath <$> Glob.globDir1 filePattern filePath

-- I/O
readFromPath :: Path Resolved File -> IO Text
readFromPath (Path filePath) = Text.IO.readFile filePath

writeToPath :: Path Resolved File -> Text -> IO ()
writeToPath (Path filePath) = Text.IO.writeFile filePath

createDirectory :: Path Resolved Dir -> IO ()
createDirectory (Path filePath) = Directory.createDirectoryIfMissing True filePath

createParent :: Path Resolved File -> IO ()
createParent = createDirectory . parent

-- Errors
data PathError
  = CouldNotFindExecutable (Path Relative File)
  | FileIsNotExecutable (Path Relative File)
  deriving (Eq, Show)

instance Exception PathError
