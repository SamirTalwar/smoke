{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)
import Data.Aeson hiding (Options)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Vector (Vector)
import Data.Yaml
import Test.Smoke.Errors
import Test.Smoke.Files
import Test.Smoke.Types

type Discovery = ExceptT SmokeDiscoveryError IO

data Root
  = DirectoryRoot Path
  | FileRoot Path
  | SingleRoot Path
               TestName

data PartialSuite = PartialSuite
  { partialSuiteWorkingDirectory :: Maybe WorkingDirectory
  , partialSuiteCommand :: Maybe Command
  , partialSuiteTests :: [Test]
  } deriving (Eq, Show)

instance FromJSON PartialSuite where
  parseJSON =
    withObject "Suite" $ \v ->
      PartialSuite <$> (v .:? "working-directory") <*> (v .:? "command") <*>
      (v .: "tests")

discoverTests :: Options -> IO TestSpecification
discoverTests options =
  TestSpecification (optionsCommand options) <$>
  discoverTestsInLocations (optionsTestLocations options)

discoverTestsInLocations :: Vector String -> IO Suites
discoverTestsInLocations locations = do
  roots <- mapM parseRoot locations
  testsBySuite <-
    forM roots $ \case
      DirectoryRoot path -> do
        specificationFiles <- findFilesInPath yamlFiles path
        mapM discoverTestsInSpecificationFile specificationFiles
      FileRoot path -> return <$> discoverTestsInSpecificationFile path
      SingleRoot path selectedTestName -> do
        let (directory, suiteName) = splitSuitePath path
        suite <-
          runExceptTIO $ do
            suite <- decodeSpecificationFile directory path
            selectedTest <-
              onNothingThrow (NoSuchTest path selectedTestName) $
              List.find ((== selectedTestName) . testName) (suiteTests suite)
            return $ suite {suiteTests = [selectedTest]}
        return [(suiteName, Right suite)]
  return $ List.sortOn fst $ concat testsBySuite

discoverTestsInSpecificationFile ::
     Path -> IO (SuiteName, Either SmokeDiscoveryError Suite)
discoverTestsInSpecificationFile path = do
  let (directory, suiteName) = splitSuitePath path
  suite <- runExceptTIO $ decodeSpecificationFile directory path
  return (suiteName, Right suite)

splitSuitePath :: Path -> (Path, SuiteName)
splitSuitePath path = (directory, SuiteName $ show $ dropExtension fileName)
  where
    (directory, fileName) = splitFileName path

decodeSpecificationFile :: Path -> Path -> Discovery Suite
decodeSpecificationFile directory path = do
  PartialSuite workingDirectory command tests <-
    do filePath <- liftIO $ unPath <$> resolvePath path
       withExceptT (InvalidSpecification path . prettyPrintParseException) .
         ExceptT $
         decodeFileEither filePath
  location <- liftIO $ resolvePath directory
  return $ Suite location workingDirectory command tests

parseRoot :: String -> IO Root
parseRoot location = do
  let (path, selectedTestName) =
        let (p, s) = List.break (== '@') location
         in ( makePath (strip p)
            , if null s
                then Nothing
                else Just (TestName (strip (tail s))))
  fileType <- resolvePath path >>= getFileType
  case (fileType, selectedTestName) of
    (NonExistentFile, _) -> throwIO $ NoSuchLocation path
    (Directory, Just selected) ->
      throwIO $ CannotSelectTestInDirectory path selected
    (Directory, Nothing) -> return $ DirectoryRoot path
    (File, Nothing) -> return $ FileRoot path
    (File, Just selected) -> return $ SingleRoot path selected

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack
