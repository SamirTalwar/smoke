{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Smoke.Types where

import Control.Exception (Exception, IOException, throwIO)
import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as Vector

data Options = Options
  { optionsCommand :: Maybe Command
  , optionsTestLocations :: [FilePath]
  } deriving (Eq, Show)

type TestName = String

type Executable = String

type Command = [String]

type Args = [String]

type Contents = Text

newtype Status = Status
  { unStatus :: Int
  } deriving (Eq, Show)

newtype StdIn = StdIn
  { unStdIn :: Contents
  } deriving (Eq, Show)

newtype StdOut = StdOut
  { unStdOut :: Contents
  } deriving (Eq, Show)

newtype StdErr = StdErr
  { unStdErr :: Contents
  } deriving (Eq, Show)

data Fixture a
  = InlineFixture a
  | FileFixture FilePath
  deriving (Eq, Show)

newtype Fixtures a =
  Fixtures [Fixture a]
  deriving (Eq, Show)

class FixtureContents a where
  fixtureName :: a -> String
  serializeFixture :: a -> Contents
  deserializeFixture :: Contents -> a

instance FixtureContents Status where
  fixtureName = const "exit-status"
  serializeFixture = Text.pack . show . unStatus
  deserializeFixture = Status . read . Text.unpack

instance FixtureContents StdIn where
  fixtureName = const "stdin"
  serializeFixture = unStdIn
  deserializeFixture = StdIn

instance FixtureContents StdOut where
  fixtureName = const "stdout"
  serializeFixture = unStdOut
  deserializeFixture = StdOut

instance FixtureContents StdErr where
  fixtureName = const "stderr"
  serializeFixture = unStdErr
  deserializeFixture = StdErr

readFixture :: FixtureContents a => Fixture a -> IO a
readFixture (InlineFixture contents) = return contents
readFixture (FileFixture path) = deserializeFixture <$> TextIO.readFile path

readFixtures :: FixtureContents a => Fixtures a -> IO [a]
readFixtures (Fixtures fixtures) = mapM readFixture fixtures

writeFixture :: FixtureContents a => Fixture a -> a -> IO ()
writeFixture (InlineFixture contents) _ =
  throwIO $
  CouldNotWriteFixture (fixtureName contents) (serializeFixture contents)
writeFixture (FileFixture path) value =
  TextIO.writeFile path (serializeFixture value)

writeFixtures ::
     forall a. FixtureContents a
  => Fixtures a
  -> a
  -> IO ()
writeFixtures (Fixtures [fixture]) value = writeFixture fixture value
writeFixtures Fixtures {} _ =
  throwIO $ CouldNotBlessWithMultipleValues (fixtureName (undefined :: a))

instance FixtureContents a => FromJSON (Fixture a) where
  parseJSON (String contents) =
    return $ InlineFixture (deserializeFixture contents)
  parseJSON (Object v) = FileFixture <$> v .: "file"
  parseJSON invalid = typeMismatch "Fixture" invalid

instance FixtureContents a => FromJSON (Fixtures a) where
  parseJSON (Array v) = Fixtures <$> mapM parseJSON (Vector.toList v)
  parseJSON v = Fixtures . return <$> (parseJSON v :: Parser (Fixture a))

data Test = Test
  { testName :: TestName
  , testLocation :: FilePath
  , testCommand :: Maybe Command
  , testArgs :: Maybe Args
  , testStdIn :: Maybe (Fixture StdIn)
  , testStdOut :: Fixtures StdOut
  , testStdErr :: Fixtures StdErr
  , testStatus :: Fixture Status
  } deriving (Eq, Show)

type Tests = [Test]

data TestExecutionPlan = TestExecutionPlan
  { planTest :: Test
  , planExecutable :: Executable
  , planArgs :: Args
  , planStdIn :: Maybe StdIn
  } deriving (Eq, Show)

type TestResults = [TestResult]

data TestResult
  = TestSuccess Test
  | TestFailure TestExecutionPlan
                (PartResult Status)
                (PartResult StdOut)
                (PartResult StdErr)
  | TestError Test
              TestErrorMessage
  deriving (Eq, Show)

data PartResult a
  = PartSuccess
  | PartFailure [a]
                a
  deriving (Eq, Show)

instance Functor PartResult where
  _ `fmap` PartSuccess = PartSuccess
  f `fmap` (PartFailure expected actual) =
    PartFailure (map f expected) (f actual)

data TestErrorMessage
  = NoCommandFile
  | NoInputFiles
  | NoOutputFiles
  | NonExistentCommand
  | NonExecutableCommand
  | CouldNotExecuteCommand String
  | CouldNotWriteFixture String
                         Text
  | BlessingFailed IOException
  | CouldNotBlessAMissingValue String
  | CouldNotBlessWithMultipleValues String
  deriving (Eq, Show)

instance Exception TestErrorMessage
