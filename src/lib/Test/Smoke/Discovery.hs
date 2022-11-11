{-# LANGUAGE LambdaCase #-}

module Test.Smoke.Discovery
  ( discoverTests,
  )
where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, throwE, withExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson.Internal
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Yaml as Yaml
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob
import Test.Smoke.Errors
import Test.Smoke.Paths
import Test.Smoke.Types

type Discovery = ExceptT SmokeDiscoveryError IO

data Root
  = DirectoryRoot (Path Relative Dir)
  | FileRoot (Path Relative File)
  | SingleRoot (Path Relative File) TestName

discoverTests :: Options -> IO TestSpecification
discoverTests options =
  runExceptTIO $
    TestSpecification (optionsCommand options)
      <$> discoverTestsInLocations (optionsTestLocations options)

discoverTestsInLocations :: Vector String -> Discovery Suites
discoverTestsInLocations locations = do
  roots <- mapM parseRoot locations
  testsBySuite <-
    forM roots $ \case
      DirectoryRoot path -> do
        specificationFiles <- liftIO $ findFilesInPath yamlFiles path
        mapM discoverTestsInSpecificationFile specificationFiles
      FileRoot path -> return <$> discoverTestsInSpecificationFile path
      SingleRoot path selectedTestName -> return <$> discoverSingleTest path selectedTestName
  return $ List.sortOn suiteMetaName $ concat testsBySuite

discoverTestsInSpecificationFile :: Path Relative File -> Discovery SuiteWithMetadata
discoverTestsInSpecificationFile path = do
  let (directory, suiteName) = splitSuitePath path
  location <- liftIO $ resolve directory
  suite <- decodeSpecificationFile path
  return $ SuiteWithMetadata suiteName location suite

discoverSingleTest :: Path Relative File -> TestName -> Discovery SuiteWithMetadata
discoverSingleTest path selectedTestName = do
  SuiteWithMetadata suiteName location fullSuite <- discoverTestsInSpecificationFile path
  selectedTest <-
    onNothingThrow (NoSuchTest path selectedTestName) $
      List.find ((== selectedTestName) . testName) (suiteTests fullSuite)
  let suite = fullSuite {suiteTests = [selectedTest]}
  return $ SuiteWithMetadata suiteName location suite

splitSuitePath :: Path Relative File -> (Path Relative Dir, SuiteName)
splitSuitePath path =
  ( parent path,
    SuiteName $ FilePath.dropExtension $ FilePath.takeFileName $ toFilePath path
  )

decodeSpecificationFile :: Path Relative File -> Discovery Suite
decodeSpecificationFile path = withExceptT (InvalidSpecification path) $ do
  resolvedPath <- liftIO $ resolve path
  value :: Aeson.Value <-
    withExceptT Yaml.prettyPrintParseException $
      ExceptT $
        Yaml.decodeFileEither (toFilePath resolvedPath)
  except $ case Aeson.Internal.ifromJSON value of
    Aeson.Internal.IError jsonPath message -> Left $ Aeson.Internal.formatError jsonPath message
    Aeson.Internal.ISuccess suite -> Right suite

parseRoot :: String -> Discovery Root
parseRoot location = do
  let (p, s) = List.break (== '@') location
  let path = strip p
  let selectedTestName =
        if null s
          then Nothing
          else Just (TestName (strip (tail s)))
  directoryExists <- liftIO $ doesDirectoryExist path
  fileExists <- liftIO $ doesFileExist path
  unless (directoryExists || fileExists) $ throwE $ NoSuchLocation path
  let parsedDir =
        if directoryExists
          then Just $ parseDir path
          else Nothing
  let parsedFile =
        if fileExists
          then Just $ parseFile path
          else Nothing
  case (parsedDir, parsedFile, selectedTestName) of
    (Just dir, Nothing, Nothing) -> return $ DirectoryRoot dir
    (Just dir, Nothing, Just selected) ->
      throwE $ CannotSelectTestInDirectory dir selected
    (Nothing, Just file, Nothing) -> return $ FileRoot file
    (Nothing, Just file, Just selected) -> return $ SingleRoot file selected
    (_, _, _) -> throwE $ NoSuchLocation path

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

yamlFiles :: Glob.Pattern
yamlFiles = Glob.compile "*.yaml"
