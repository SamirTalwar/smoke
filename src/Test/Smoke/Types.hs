module Test.Smoke.Types where

import Control.Exception (Exception, IOException, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

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
readFixture (InlineFixture fixture) = return fixture
readFixture (FileFixture path) = deserializeFixture <$> TextIO.readFile path

writeFixture :: FixtureContents a => Fixture a -> a -> IO ()
writeFixture (InlineFixture value) _ =
  throwIO $ CouldNotWriteFixture (fixtureName value) (serializeFixture value)
writeFixture (FileFixture path) value =
  TextIO.writeFile path (serializeFixture value)

data Test = Test
  { testName :: TestName
  , testLocation :: FilePath
  , testCommand :: Maybe Command
  , testArgs :: Maybe Args
  , testStdIn :: Maybe (Fixture StdIn)
  , testStdOut :: [Fixture StdOut]
  , testStdErr :: [Fixture StdErr]
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
  | CouldNotBlessStdOutWithMultipleValues
  | CouldNotBlessStdErrWithMultipleValues
  deriving (Eq, Show)

instance Exception TestErrorMessage
