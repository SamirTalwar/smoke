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

newtype SuiteName = SuiteName
  { unSuiteName :: String
  }
  deriving (Eq, Ord, Show)

newtype TestName = TestName
  { unTestName :: String
  }
  deriving (Eq, Ord, Show)

newtype WorkingDirectory = WorkingDirectory
  { unWorkingDirectory :: ResolvedPath Dir
  }
  deriving (Eq, Show)

newtype Args = Args
  { unArgs :: Vector String
  }
  deriving (Eq, Show, Semigroup, Monoid, FromJSON)

newtype Script = Script
  { unScript :: Text
  }
  deriving (Eq, Show, FromJSON)

data CommandLine
  = CommandLine (RelativePath File) Args
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

newtype FixtureName = FixtureName
  { unFixtureName :: String
  }
  deriving (Eq, Show, IsString)

class FixtureType a where
  fixtureName :: FixtureName
  serializeFixture :: a -> Text
  deserializeFixture :: Text -> a

parseFixtureJSON :: forall a. FixtureType a => Value -> Parser a
parseFixtureJSON = withText (unFixtureName (fixtureName @a)) (return . deserializeFixture)

instance FixtureType Text where
  fixtureName = "text"
  serializeFixture = id
  deserializeFixture = id

newtype Status = Status
  { unStatus :: Int
  }
  deriving (Eq, Show)

instance Default Status where
  def = Status 0

instance FromJSON Status where
  parseJSON number@(Number _) = Status <$> parseJSON number
  parseJSON invalid = typeMismatch "status" invalid

newtype StdIn = StdIn
  { unStdIn :: Text
  }
  deriving (Eq, Show)

instance Default StdIn where
  def = StdIn Text.empty

instance FixtureType StdIn where
  fixtureName = "stdin"
  serializeFixture = unStdIn
  deserializeFixture = StdIn

instance FromJSON StdIn where
  parseJSON = parseFixtureJSON

newtype StdOut = StdOut
  { unStdOut :: Text
  }
  deriving (Eq, Show)

instance Default StdOut where
  def = StdOut Text.empty

instance FixtureType StdOut where
  fixtureName = "stdout"
  serializeFixture = unStdOut
  deserializeFixture = StdOut

instance FromJSON StdOut where
  parseJSON = parseFixtureJSON

newtype StdErr = StdErr
  { unStdErr :: Text
  }
  deriving (Eq, Show)

instance Default StdErr where
  def = StdErr Text.empty

instance FixtureType StdErr where
  fixtureName = "stderr"
  serializeFixture = unStdErr
  deserializeFixture = StdErr

instance FromJSON StdErr where
  parseJSON = parseFixtureJSON

newtype TestFileContents = TestFileContents
  { unTestFileContents :: Text
  }
  deriving (Eq, Show)

instance FixtureType TestFileContents where
  fixtureName = "files"
  serializeFixture = unTestFileContents
  deserializeFixture = TestFileContents

instance FromJSON TestFileContents where
  parseJSON = parseFixtureJSON
