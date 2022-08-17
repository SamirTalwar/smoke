{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Smoke.Types.Base where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Default
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Smoke.Paths

newtype FixtureName = FixtureName {unFixtureName :: String}
  deriving (Eq, Show, IsString)

class Eq a => IsFixture a where
  fixtureName :: FixtureName

class IsFixture a => ToFixture a where
  deserializeFixture :: Text -> a

class IsFixture a => FromFixture a where
  serializeFixture :: a -> Text

parseFixtureJSON :: forall a. ToFixture a => Value -> Parser a
parseFixtureJSON = withText (unFixtureName (fixtureName @a)) (return . deserializeFixture)

instance IsFixture Text where
  fixtureName = "text"

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

instance IsFixture Args where
  fixtureName = "args"

instance FromFixture Args where
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
      return $
        CommandLine (parseFile (Vector.head line)) (Args (Vector.tail line))

data Command
  = CommandArgs CommandLine
  | CommandScript (Maybe CommandLine) Script
  deriving (Eq, Show)

instance FromJSON Command where
  parseJSON (Object v) = CommandScript <$> v .:? "shell" <*> v .: "script"
  parseJSON (String script) = return $ CommandScript Nothing (Script script)
  parseJSON args@(Array _) = CommandArgs <$> parseJSON args
  parseJSON invalid = typeMismatch "command" invalid

newtype Status = Status
  { unStatus :: Int
  }
  deriving (Eq, Show)

instance Default Status where
  def = Status 0

instance IsFixture Status where
  fixtureName = "status"

instance FromFixture Status where
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

instance IsFixture StdIn where
  fixtureName = "stdin"

instance ToFixture StdIn where
  deserializeFixture = StdIn

instance FromFixture StdIn where
  serializeFixture = unStdIn

instance FromJSON StdIn where
  parseJSON = parseFixtureJSON

newtype StdOut = StdOut
  { unStdOut :: Text
  }
  deriving (Eq, Show)

instance Default StdOut where
  def = StdOut Text.empty

instance IsFixture StdOut where
  fixtureName = "stdout"

instance ToFixture StdOut where
  deserializeFixture = StdOut

instance FromFixture StdOut where
  serializeFixture = unStdOut

instance FromJSON StdOut where
  parseJSON = parseFixtureJSON

newtype StdErr = StdErr
  { unStdErr :: Text
  }
  deriving (Eq, Show)

instance Default StdErr where
  def = StdErr Text.empty

instance IsFixture StdErr where
  fixtureName = "stderr"

instance ToFixture StdErr where
  deserializeFixture = StdErr

instance FromFixture StdErr where
  serializeFixture = unStdErr

instance FromJSON StdErr where
  parseJSON = parseFixtureJSON

newtype TestFileContents = TestFileContents
  { unTestFileContents :: Text
  }
  deriving (Eq, Show)

instance IsFixture TestFileContents where
  fixtureName = "files"

instance ToFixture TestFileContents where
  deserializeFixture = TestFileContents

instance FromFixture TestFileContents where
  serializeFixture = unTestFileContents

instance FromJSON TestFileContents where
  parseJSON = parseFixtureJSON
