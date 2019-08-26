{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Smoke.Paths
  ( File
  , Dir
  , Path
  , RelativePath
  , ResolvedPath
  , (</>)
  , findFilesInPath
  , getCurrentWorkingDirectory
  , parent
  , parseDir
  , parseFile
  , readFromPath
  , resolve
  , toFilePath
  , writeToPath
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob

data Dir

data File

newtype RelativePath t =
  RelativePath FilePath
  deriving (Eq, Ord, Show)

newtype ResolvedPath t =
  ResolvedPath FilePath
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

parseDir :: FilePath -> RelativePath Dir
parseDir = fromFilePath

parseFile :: FilePath -> RelativePath File
parseFile = fromFilePath

-- Query
parent :: (Path p t, Path q Dir, p ~ q) => p t -> q Dir
parent = fromFilePath . FilePath.dropFileName . toFilePath

-- Resolve
(</>) ::
     (Path RelativePath t, Path ResolvedPath t)
  => ResolvedPath Dir
  -> RelativePath t
  -> ResolvedPath t
ResolvedPath a </> RelativePath b = fromFilePath (a FilePath.</> b)

resolve ::
     (Path RelativePath t, Path ResolvedPath t)
  => RelativePath t
  -> IO (ResolvedPath t)
resolve path = do
  currentWorkingDirectory <- getCurrentWorkingDirectory
  return $ currentWorkingDirectory </> path

normalizeFilePath :: FilePath -> FilePath
normalizeFilePath =
  FilePath.joinPath .
  interpretParentAccess .
  removeExtraSeparators . FilePath.splitPath . FilePath.normalise
  where
    removeExtraSeparators :: [FilePath] -> [FilePath]
    removeExtraSeparators = map removeExtraSeparator
    removeExtraSeparator :: FilePath -> FilePath
    removeExtraSeparator segment =
      let (name, separators) = span (`notElem` FilePath.pathSeparators) segment
       in name ++ take 1 separators
    interpretParentAccess :: [FilePath] -> [FilePath]
    interpretParentAccess [] = []
    interpretParentAccess [x] = [x]
    interpretParentAccess (x:y:rest) =
      if FilePath.normalise y == ".."
        then interpretParentAccess rest
        else x : interpretParentAccess (y : rest)

-- Search
getCurrentWorkingDirectory :: IO (ResolvedPath Dir)
getCurrentWorkingDirectory = ResolvedPath <$> Directory.getCurrentDirectory

findFilesInPath ::
     (Path p Dir, Path q File, p ~ q) => Glob.Pattern -> p Dir -> IO [q File]
findFilesInPath filePattern path =
  map fromFilePath <$> Glob.globDir1 filePattern (toFilePath path)

-- I/O
readFromPath :: ResolvedPath File -> IO Text
readFromPath = Text.IO.readFile . toFilePath

writeToPath :: ResolvedPath File -> Text -> IO ()
writeToPath = Text.IO.writeFile . toFilePath
