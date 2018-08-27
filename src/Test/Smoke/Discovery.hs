{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Discovery
  ( discoverTests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Maybe (maybe)
import Data.Yaml
import System.FilePath
import System.FilePath.Glob as Glob
import Test.Smoke.Types

data TestSuite =
  TestSuite (Maybe Command)
            [TestSpecification]

data TestSpecification =
  TestSpecification TestName
                    (Maybe Command)
                    (Maybe Args)
                    (Maybe (Fixture StdIn))
                    (Fixtures StdOut)
                    (Fixtures StdErr)
                    (Fixture Status)

instance FromJSON TestSuite where
  parseJSON =
    withObject "TestSuite" $ \v ->
      TestSuite <$> (v .:? "command") <*> (v .: "tests")

instance FromJSON TestSpecification where
  parseJSON =
    withObject "TestSpecification" $ \v ->
      TestSpecification <$> (v .: "name") <*> (v .:? "command") <*>
      (v .:? "args") <*>
      (v .:? "stdin") <*>
      (v .:? "stdout" .!= Fixtures []) <*>
      (v .:? "stderr" .!= Fixtures []) <*>
      (InlineFixture . Status <$> v .:? "exit-status" .!= 0)

discoverTests :: Options -> IO Tests
discoverTests options =
  concat <$>
  forM
    (optionsTestLocations options)
    (discoverTestsInLocation (optionsCommand options))

discoverTestsInLocation :: Maybe Command -> FilePath -> IO [Test]
discoverTestsInLocation commandFromOptions location = do
  specificationFiles <- globDir1 (Glob.compile "*.yaml") location
  testsBySuite <-
    forM specificationFiles $ \file -> do
      let suiteName =
            if length specificationFiles > 1
              then Just $ makeRelative location (dropExtension file)
              else Nothing
      suite <- decodeFileThrow file
      return $ convertToTests commandFromOptions location suiteName suite
  return $ concat testsBySuite

convertToTests ::
     Maybe Command -> FilePath -> Maybe TestName -> TestSuite -> Tests
convertToTests commandFromOptions location suiteName (TestSuite suiteCommand specs) =
  map
    (convertToTest location suiteName (commandFromOptions <|> suiteCommand))
    specs

convertToTest ::
     FilePath -> Maybe TestName -> Maybe Command -> TestSpecification -> Test
convertToTest location suiteName suiteCommand (TestSpecification name command args stdIn stdOut stdErr status) =
  Test
    { testName = maybe name (++ "/" ++ name) suiteName
    , testLocation = location
    , testCommand = command <|> suiteCommand
    , testArgs = args
    , testStdIn = prefixFixtureWith location <$> stdIn
    , testStdOut = prefixFixturesWith location stdOut
    , testStdErr = prefixFixturesWith location stdErr
    , testStatus = status
    }

prefixFixtureWith :: FilePath -> Fixture a -> Fixture a
prefixFixtureWith _ fixture@InlineFixture {} = fixture
prefixFixtureWith location (FileFixture path) = FileFixture (location </> path)

prefixFixturesWith :: FilePath -> Fixtures a -> Fixtures a
prefixFixturesWith location (Fixtures fixtures) =
  Fixtures $ map (prefixFixtureWith location) fixtures
