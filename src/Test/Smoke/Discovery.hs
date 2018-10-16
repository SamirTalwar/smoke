{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Vector (Vector)
import Data.Yaml
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
import System.FilePath.Glob as Glob
import Test.Smoke.Errors
import Test.Smoke.Types

type Discovery = ExceptT TestDiscoveryErrorMessage IO

data Root
  = Directory FilePath
  | File FilePath
  | Single FilePath
           TestName

discoverTests :: Options -> IO TestSpecification
discoverTests options =
  TestSpecification (optionsCommand options) <$>
  discoverTestsInLocations (optionsTestLocations options)

discoverTestsInLocations :: Vector FilePath -> IO Suites
discoverTestsInLocations locations = do
  roots <- mapM parseRoot locations
  testsBySuite <-
    forM roots $ \case
      Directory path -> do
        specificationFiles <- globDir1 (Glob.compile "*.yaml") path
        mapM discoverTestsInSpecificationFile specificationFiles
      File path -> return <$> discoverTestsInSpecificationFile path
      Single path selectedTestName -> do
        let (directory, suiteName) = splitSuitePath path
        suite <-
          runExceptT $ do
            Suite command tests <- decodeSpecificationFile directory path
            selectedTest <-
              onNothingThrow (NoSuchTest path selectedTestName) $
              List.find ((== selectedTestName) . testName) tests
            return $ Suite command [selectedTest]
        return [(suiteName, suite)]
  return $ List.sortOn fst $ concat testsBySuite

splitSuitePath :: FilePath -> (FilePath, SuiteName)
splitSuitePath path = (directory, SuiteName $ dropExtension fileName)
  where
    (directory, fileName) = splitFileName path

discoverTestsInSpecificationFile ::
     FilePath -> IO (SuiteName, Either TestDiscoveryErrorMessage Suite)
discoverTestsInSpecificationFile path = do
  let (directory, suiteName) = splitSuitePath path
  suite <- runExceptT $ decodeSpecificationFile directory path
  return (suiteName, suite)

decodeSpecificationFile :: FilePath -> FilePath -> Discovery Suite
decodeSpecificationFile directory path =
  withExceptT (InvalidSpecification path . prettyPrintParseException) $
  prefixSuiteFixturesWith (dropTrailingPathSeparator directory) <$>
  ExceptT (decodeFileEither path)

prefixSuiteFixturesWith :: FilePath -> Suite -> Suite
prefixSuiteFixturesWith location (Suite command tests) =
  Suite command (map (prefixTestFixturesWith location) tests)

prefixTestFixturesWith :: FilePath -> Test -> Test
prefixTestFixturesWith location test =
  test
    { testStdIn = prefixFixtureWith location <$> testStdIn test
    , testStdOut = prefixFixturesWith location $ testStdOut test
    , testStdErr = prefixFixturesWith location $ testStdErr test
    }

prefixFixtureWith :: FilePath -> Fixture a -> Fixture a
prefixFixtureWith _ fixture@InlineFixture {} = fixture
prefixFixtureWith location (FileFixture path) = FileFixture (location </> path)

prefixFixturesWith :: FilePath -> Fixtures a -> Fixtures a
prefixFixturesWith location (Fixtures fixtures) =
  Fixtures $ prefixFixtureWith location <$> fixtures

parseRoot :: FilePath -> IO Root
parseRoot location = do
  let (path, selectedTestName) =
        let (p, s) = List.break (== '@') location
         in ( strip p
            , if null s
                then Nothing
                else Just (TestName (strip (tail s))))
  isDirectory <- doesDirectoryExist path
  isFile <- doesFileExist path
  case (isDirectory, isFile, selectedTestName) of
    (True, True, _) ->
      fail $ "The path \"" ++ path ++ "\" is both a directory and a file."
    (False, False, _) -> throwIO $ NoSuchLocation path
    (True, _, Just selected) ->
      throwIO $ CannotSelectTestInDirectory path selected
    (True, False, Nothing) -> return $ Directory path
    (False, True, Nothing) -> return $ File path
    (False, True, Just selected) -> return $ Single path selected

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack
