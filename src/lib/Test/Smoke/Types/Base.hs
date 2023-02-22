{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Base where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Default
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Test.Smoke.Paths

newtype FixtureName = FixtureName {unFixtureName :: String}
  deriving (Eq, Show, IsString)

class ToFixture a where
  deserializeFixture :: Text -> a

class FromFixture a where
  fixtureName :: FixtureName
  serializeFixture :: a -> Text

parseFixtureJSON :: ToFixture a => String -> Value -> Parser a
parseFixtureJSON name = withText name (return . deserializeFixture)

instance ToFixture Text where
  deserializeFixture = id

newtype SuiteName = SuiteName
  { unSuiteName :: String
  }
  deriving (Eq, Ord, Show)

newtype TestName = TestName
  { unTestName :: String
  }
  deriving (Eq, Ord, Show)

newtype WorkingDirectory = WorkingDirectory
  { unWorkingDirectory :: Path Resolved Dir
  }
  deriving (Eq, Show)

newtype Args = Args
  { unArgs :: Vector String
  }
  deriving (Eq, Show, Semigroup, Monoid, FromJSON)

instance FromFixture Args where
  fixtureName = "args"
  serializeFixture = Text.unlines . Vector.toList . Vector.map Text.pack . unArgs

newtype Script = Script
  { unScript :: Text
  }
  deriving (Eq, Show, FromJSON)

data CommandLine
  = CommandLine (Path Relative File) Args
  deriving (Eq, Show)

instance FromJSON CommandLine where
  parseJSON =
    withArray "command line" $ \v -> do
      line <- mapM parseJSON v
      when (Vector.null line) $ fail "empty command line"
      pure $
        CommandLine (parseFile (Vector.head line)) (Args (Vector.tail line))

data Command
  = CommandArgs CommandLine
  | CommandScript (Maybe CommandLine) Script
  deriving (Eq, Show)

instance FromJSON Command where
  parseJSON (Object v) = CommandScript <$> v .:? "shell" <*> v .: "script"
  parseJSON (String script) = pure $ CommandScript Nothing (Script script)
  parseJSON args@(Array _) = CommandArgs <$> parseJSON args
  parseJSON invalid = typeMismatch "command" invalid

newtype Status = Status
  { unStatus :: Int
  }
  deriving (Eq, Show)

instance Default Status where
  def = Status 0

instance FromFixture Status where
  fixtureName = "status"
  serializeFixture = (<> "\n") . Text.pack . show . unStatus

instance FromJSON Status where
  parseJSON number@(Number _) = Status <$> parseJSON number
  parseJSON invalid = typeMismatch "status" invalid

newtype StdIn = StdIn
  { unStdIn :: Text
  }
  deriving (Eq, Show)

instance Default StdIn where
  def = StdIn Text.empty

instance ToFixture StdIn where
  deserializeFixture = StdIn

instance FromFixture StdIn where
  fixtureName = "stdin"
  serializeFixture = unStdIn

instance FromJSON StdIn where
  parseJSON = parseFixtureJSON "stdin"

newtype StdOut = StdOut
  { unStdOut :: Text
  }
  deriving (Eq, Show)

instance Default StdOut where
  def = StdOut Text.empty

instance ToFixture StdOut where
  deserializeFixture = StdOut

instance FromFixture StdOut where
  fixtureName = "stdout"
  serializeFixture = unStdOut

instance FromJSON StdOut where
  parseJSON = parseFixtureJSON "stdout"

newtype StdErr = StdErr
  { unStdErr :: Text
  }
  deriving (Eq, Show)

instance Default StdErr where
  def = StdErr Text.empty

instance ToFixture StdErr where
  deserializeFixture = StdErr

instance FromFixture StdErr where
  fixtureName = "stderr"
  serializeFixture = unStdErr

instance FromJSON StdErr where
  parseJSON = parseFixtureJSON "stderr"

newtype TestFileContents = TestFileContents
  { unTestFileContents :: Text
  }
  deriving (Eq, Show)

instance ToFixture TestFileContents where
  deserializeFixture = TestFileContents

instance FromFixture TestFileContents where
  fixtureName = "files"
  serializeFixture = unTestFileContents

instance FromJSON TestFileContents where
  parseJSON = parseFixtureJSON "file"
