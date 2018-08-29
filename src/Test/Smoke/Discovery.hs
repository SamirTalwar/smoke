{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Yaml
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
import System.FilePath.Glob as Glob
import Test.Smoke.Types

data Root
  = Directory FilePath
  | File FilePath
  | Single FilePath
           TestName

discoverTests :: Options -> IO Plan
discoverTests options =
  Plan (optionsCommand options) <$>
  discoverTestsInLocations (optionsTestLocations options)

discoverTestsInLocations :: [FilePath] -> IO Suites
discoverTestsInLocations locations = do
  roots <- mapM parseRoot locations
  testsBySuite <-
    forM roots $ \case
      Directory path -> do
        specificationFiles <- globDir1 (Glob.compile "*.yaml") path
        mapM decodeSpecificationFile specificationFiles
      File path -> return <$> decodeSpecificationFile path
      Single path selectedTestName -> do
        (suiteName, Suite command tests) <- decodeSpecificationFile path
        case List.find ((== selectedTestName) . testName) tests of
          Nothing -> throwIO (NoSuchTest path selectedTestName)
          Just selectedTest ->
            return [(suiteName, Suite command [selectedTest])]
  return $ List.sortOn fst $ concat testsBySuite

decodeSpecificationFile :: FilePath -> IO (SuiteName, Suite)
decodeSpecificationFile file = do
  let (directory, fileName) = splitFileName file
  let suiteName = dropExtension fileName
  suite <- decodeFileThrow file
  return
    ( suiteName
    , prefixSuiteFixturesWith (dropTrailingPathSeparator directory) suite)

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
  Fixtures $ map (prefixFixtureWith location) fixtures

parseRoot :: FilePath -> IO Root
parseRoot location = do
  let (path, selectedTestName) =
        let (p, s) = List.break (== '@') location
         in ( strip p
            , if null s
                then Nothing
                else Just (strip (tail s)))
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
