{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM)
import Data.List (sortOn)
import Data.Yaml
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
import System.FilePath.Glob as Glob
import Test.Smoke.Types

discoverTests :: Options -> IO Plan
discoverTests options =
  Plan (optionsCommand options) <$>
  forM (optionsTestLocations options) discoverTestsInLocation

discoverTestsInLocation :: FilePath -> IO Suites
discoverTestsInLocation location = do
  locationType <- getFileType location
  specificationFiles <-
    case locationType of
      Directory -> globDir1 (Glob.compile "*.yaml") location
      File -> return [location]
      NonExistent -> throwIO (NoSuchLocation location)
  testsBySuite <-
    forM specificationFiles $ \file -> do
      let suiteName =
            if length specificationFiles > 1
              then Just $ makeRelative location (dropExtension file)
              else Nothing
      suite <- decodeFileThrow file
      return (suiteName, prefixSuiteFixturesWith location suite)
  return $ sortOn fst testsBySuite

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

data FileType
  = Directory
  | File
  | NonExistent

getFileType :: FilePath -> IO FileType
getFileType path = do
  isDirectory <- doesDirectoryExist path
  isFile <- doesFileExist path
  case (isDirectory, isFile) of
    (True, True) ->
      fail $ "The path \"" ++ path ++ "\" is both a directory and a file."
    (True, False) -> return Directory
    (False, True) -> return File
    (False, False) -> return NonExistent
