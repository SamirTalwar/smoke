{-# LANGUAGE LambdaCase #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)
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
            Suite workingDirectory command tests <-
              decodeSpecificationFile directory path
            selectedTest <-
              onNothingThrow (NoSuchTest path selectedTestName) $
              List.find ((== selectedTestName) . testName) tests
            return $ Suite workingDirectory command [selectedTest]
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
decodeSpecificationFile directory path =
  withExceptT (InvalidSpecification path . prettyPrintParseException) $
  prefixSuiteFixturesWith directory <$> ExceptT (decodeFileEither (show path))

prefixSuiteFixturesWith :: Path -> Suite -> Suite
prefixSuiteFixturesWith location (Suite workingDirectory command tests) =
  Suite workingDirectory command (map (prefixTestFixturesWith location) tests)

prefixTestFixturesWith :: Path -> Test -> Test
prefixTestFixturesWith location test =
  test
    { testStdIn = prefixFixtureWith location <$> testStdIn test
    , testStdOut = prefixFixturesWith location $ testStdOut test
    , testStdErr = prefixFixturesWith location $ testStdErr test
    }

prefixFixtureWith :: Path -> Fixture a -> Fixture a
prefixFixtureWith _ fixture@(Fixture (Inline _) _) = fixture
prefixFixtureWith location (Fixture (FileLocation path) fixtureFilter) =
  Fixture (FileLocation (location </> path)) fixtureFilter

prefixFixturesWith :: Path -> Fixtures a -> Fixtures a
prefixFixturesWith location (Fixtures fixtures) =
  Fixtures $ prefixFixtureWith location <$> fixtures

parseRoot :: String -> IO Root
parseRoot location = do
  let (path, selectedTestName) =
        let (p, s) = List.break (== '@') location
         in ( makePath (strip p)
            , if null s
                then Nothing
                else Just (TestName (strip (tail s))))
  fileType <- getFileType path
  case (fileType, selectedTestName) of
    (NonExistentFile, _) -> throwIO $ NoSuchLocation path
    (Directory, Just selected) ->
      throwIO $ CannotSelectTestInDirectory path selected
    (Directory, Nothing) -> return $ DirectoryRoot path
    (File, Nothing) -> return $ FileRoot path
    (File, Just selected) -> return $ SingleRoot path selected

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack
