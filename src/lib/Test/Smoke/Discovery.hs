{-# LANGUAGE LambdaCase #-}

module Test.Smoke.Discovery
  ( discoverTests,
  )
where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, throwE, withExceptT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Yaml qualified as Yaml
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath qualified as FilePath
import System.FilePath.Glob qualified as Glob
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
      FileRoot path -> pure <$> discoverTestsInSpecificationFile path
      SingleRoot path selectedTestName -> pure <$> discoverSingleTest path selectedTestName
  pure $ List.sortOn suiteMetaName $ concat testsBySuite

discoverTestsInSpecificationFile :: Path Relative File -> Discovery SuiteWithMetadata
discoverTestsInSpecificationFile path = do
  let (directory, suiteName) = splitSuitePath path
  location <- liftIO $ resolve directory
  suite <- decodeSpecificationFile path
  pure $ SuiteWithMetadata suiteName location suite

discoverSingleTest :: Path Relative File -> TestName -> Discovery SuiteWithMetadata
discoverSingleTest path selectedTestName = do
  SuiteWithMetadata suiteName location fullSuite <- discoverTestsInSpecificationFile path
  selectedTest <-
    onNothingThrow (NoSuchTest path selectedTestName) $
      List.find ((== selectedTestName) . testName) (suiteTests fullSuite)
  let suite = fullSuite {suiteTests = [selectedTest]}
  pure $ SuiteWithMetadata suiteName location suite

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
  except $ case Aeson.Types.ifromJSON value of
    Aeson.Types.IError jsonPath message -> Left $ Aeson.Types.formatError jsonPath message
    Aeson.Types.ISuccess suite -> Right suite

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
    (Just dir, Nothing, Nothing) -> pure $ DirectoryRoot dir
    (Just dir, Nothing, Just selected) ->
      throwE $ CannotSelectTestInDirectory dir selected
    (Nothing, Just file, Nothing) -> pure $ FileRoot file
    (Nothing, Just file, Just selected) -> pure $ SingleRoot file selected
    (_, _, _) -> throwE $ NoSuchLocation path

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

yamlFiles :: Glob.Pattern
yamlFiles = Glob.compile "*.yaml"
