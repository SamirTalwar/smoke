{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, liftM2, mzero)
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find, groupBy, sortBy)
import Data.Maybe (maybe)
import Data.Yaml
import System.Directory
import System.FilePath
import System.FilePath.Glob as Glob
import Test.Smoke.FileTypes (FileType)
import qualified Test.Smoke.FileTypes as FileTypes
import Test.Smoke.Types

newtype TestSuite =
  TestSuite [TestSpecification]

data TestSpecification = TestSpecification
  { specName :: TestName
  , specArgs :: Maybe Args
  , specStdOut :: TestSpecificationFile
  , specStatus :: Status
  }

newtype TestSpecificationFile =
  TestSpecificationFile FilePath

instance FromJSON TestSuite where
  parseJSON =
    withObject "TestSuite" $ \v -> do
      testObjects <- v .: "tests"
      TestSuite <$>
        forM (HashMap.toList testObjects) (uncurry parseTestSpecification)

discoverTests :: Options -> IO Tests
discoverTests options =
  concat <$>
  forM
    (optionsTestLocations options)
    (discoverTestsInLocation (optionsCommand options))

discoverTestsInLocation :: Maybe Command -> FilePath -> IO [Test]
discoverTestsInLocation commandFromOptions location = do
  specifications <-
    discoverTestSpecificationsInLocation commandFromOptions location
  byGlob <- discoverTestsByGlobInLocation commandFromOptions location
  return $ specifications ++ byGlob

discoverTestSpecificationsInLocation :: Maybe Command -> FilePath -> IO [Test]
discoverTestSpecificationsInLocation commandFromOptions location = do
  specificationFiles <- globDir1 (Glob.compile "*.yaml") location
  testsBySuite <-
    forM specificationFiles $ \file -> do
      let suiteName = makeRelative location $ dropExtension file
      suite <- decodeFileThrow file
      return $ convertToTests commandFromOptions location suiteName suite
  return $ concat testsBySuite

discoverTestsByGlobInLocation :: Maybe Command -> FilePath -> IO [Test]
discoverTestsByGlobInLocation commandFromOptions location = do
  isDirectory <- doesDirectoryExist location
  let directory =
        if isDirectory
          then location
          else takeDirectory location
  let globs =
        if isDirectory
          then FileTypes.directoryGlobs
          else FileTypes.fileGlobs (takeFileName location)
  (command, files) <- discoverFilesByGlob commandFromOptions directory globs
  groupTests directory command files

discoverFilesByGlob ::
     Maybe Command
  -> FilePath
  -> [(FileType, Pattern)]
  -> IO (Maybe Command, [(FileType, FilePath)])
discoverFilesByGlob commandFromOptions directory globs = do
  command <- findCommand
  files <- allFiles
  return (command, files)
  where
    findCommand =
      return commandFromOptions <<|>>
      readCommandFileIfExists (directory </> "command")
    allFiles =
      sortBy (compare `on` snd) .
      concat .
      zipWith
        (\fileTypeGlob paths -> zip (repeat fileTypeGlob) paths)
        (map fst globs) <$>
      globDir (map snd globs) directory

groupTests :: FilePath -> Maybe Command -> [(FileType, FilePath)] -> IO [Test]
groupTests directory command files = do
  let grouped = groupBy ((==) `on` (dropExtension . snd)) files
  forM grouped (constructTestFromGroup directory command)

constructTestFromGroup ::
     FilePath -> Maybe Command -> [(FileType, FilePath)] -> IO Test
constructTestFromGroup location commandForLocation group = do
  let part fileType = snd <$> find ((== fileType) . fst) group
  let parts fileType = snd <$> filter ((== fileType) . fst) group
  let name = makeRelative location $ dropExtension (snd (head group))
  command <-
    sequence (readCommandFile <$> part FileTypes.Command) <<|>>
    return commandForLocation
  args <- sequence (readCommandFile <$> part FileTypes.Args)
  let stdIn = part FileTypes.StdIn
  let stdOut = parts FileTypes.StdOut
  let stdErr = parts FileTypes.StdErr
  status <- Status <$> maybe (return 0) readStatusFile (part FileTypes.Status)
  return
    Test
      { testName = name
      , testLocation = location
      , testCommand = command
      , testArgs = args
      , testStdIn = stdIn
      , testStdOut = stdOut
      , testStdErr = stdErr
      , testStatus = status
      }

readCommandFileIfExists :: FilePath -> IO (Maybe Command)
readCommandFileIfExists path = do
  exists <- doesFileExist path
  if exists
    then Just <$> readCommandFile path
    else return Nothing

readCommandFile :: FilePath -> IO Command
readCommandFile path = lines <$> readFile path

readStatusFile :: FilePath -> IO Int
readStatusFile path = read <$> readFile path

parseTestSpecification :: TestName -> Value -> Parser TestSpecification
parseTestSpecification name (Object spec) =
  TestSpecification name <$> (spec .:? "args") <*> (spec .: "stdout") <*>
  (Status <$> spec .:? "exit-status" .!= 0)
parseTestSpecification _ _ = mzero

instance FromJSON TestSpecificationFile where
  parseJSON =
    withObject "TestSpecificationFile" $ \v ->
      TestSpecificationFile <$> v .: "file"

convertToTests :: Maybe Command -> FilePath -> TestName -> TestSuite -> Tests
convertToTests commandFromOptions location suiteName (TestSuite specs) =
  map (convertToTest commandFromOptions location suiteName) specs

convertToTest ::
     Maybe Command -> FilePath -> TestName -> TestSpecification -> Test
convertToTest commandFromOptions location suiteName TestSpecification { specName = name
                                                                      , specArgs = args
                                                                      , specStdOut = (TestSpecificationFile stdOut)
                                                                      , specStatus = status
                                                                      } =
  Test
    { testName = suiteName ++ "/" ++ name
    , testLocation = location
    , testCommand = commandFromOptions
    , testArgs = args
    , testStdIn = Nothing
    , testStdOut = [location </> stdOut]
    , testStdErr = []
    , testStatus = status
    }

(<<|>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
(<<|>>) = liftM2 (<|>)

infixl 3 <<|>>
