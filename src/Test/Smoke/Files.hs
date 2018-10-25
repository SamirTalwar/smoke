module Test.Smoke.Files
  ( (</>)
  , dropExtension
  , findFilesInPath
  , getFileType
  , readFromPath
  , splitFileName
  , writeToPath
  , yamlFiles
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob
import Test.Smoke.Types

-- Query
getFileType :: Path -> IO FileType
getFileType path = do
  let filePath = unPath path
  isDirectory <- Directory.doesDirectoryExist filePath
  isFile <- Directory.doesFileExist filePath
  case (isDirectory, isFile) of
    (True, True) ->
      fail $ "The path \"" ++ show path ++ "\" is both a directory and a file."
    (True, False) -> return Directory
    (False, True) -> return File
    (False, False) -> return NonExistentFile

-- Search
findFilesInPath :: Glob.Pattern -> Path -> IO [Path]
findFilesInPath filePattern (AbsolutePath directory) =
  map AbsolutePath <$> Glob.globDir1 filePattern directory
findFilesInPath filePattern (RelativePath directory) =
  map RelativePath <$> Glob.globDir1 filePattern directory

yamlFiles :: Glob.Pattern
yamlFiles = Glob.compile "*.yaml"

-- Manipulation
(</>) :: Path -> Path -> Path
_ </> suffix@(AbsolutePath _) = suffix
(AbsolutePath prefix) </> (RelativePath suffix) =
  AbsolutePath $ prefix FilePath.</> suffix
(RelativePath prefix) </> (RelativePath suffix) =
  RelativePath $ prefix FilePath.</> suffix

dropExtension :: FileName -> FileName
dropExtension = FileName . FilePath.dropExtension . unFileName

splitFileName :: Path -> (Path, FileName)
splitFileName (AbsolutePath path) =
  let (directory, fileName) = FilePath.splitFileName path
   in (AbsolutePath directory, FileName fileName)
splitFileName (RelativePath path) =
  let (directory, fileName) = FilePath.splitFileName path
   in (RelativePath directory, FileName fileName)

-- I/O
readFromPath :: Path -> IO Text
readFromPath = TextIO.readFile . unPath

writeToPath :: Path -> Text -> IO ()
writeToPath path = TextIO.writeFile $ unPath path

-- Utilities
unPath :: Path -> FilePath
unPath (AbsolutePath value) = value
unPath (RelativePath value) = value

unFileName :: FileName -> String
unFileName (FileName value) = value
