{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Base where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Default
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Path
import Test.Smoke.Shell
import Test.Smoke.Types.Args
import Test.Smoke.Types.Shell

data Contents a
  = Inline a
  | FileLocation (Path Rel File)
  deriving (Eq, Show)

parseContents :: (Text -> a) -> Value -> Parser (Contents a)
parseContents deserialize (String contents) =
  return $ Inline (deserialize contents)
parseContents deserialize (Object v) = do
  maybeContents <- v .:? "contents"
  maybeFile <- v .:? "file"
  case (maybeContents, maybeFile) of
    (Just _, Just _) -> fail "Expected \"contents\" or a \"file\", not both."
    (Just contents, Nothing) -> return $ Inline (deserialize contents)
    (Nothing, Just file) -> return $ FileLocation file
    (Nothing, Nothing) -> fail "Expected \"contents\" or a \"file\"."
parseContents _ invalid = typeMismatch "contents" invalid

newtype SuiteName =
  SuiteName
    { unSuiteName :: String
    }
  deriving (Eq, Ord, Show)

newtype TestName =
  TestName
    { unTestName :: String
    }
  deriving (Eq, Ord, Show)

newtype WorkingDirectory =
  WorkingDirectory
    { unWorkingDirectory :: Path Abs Dir
    }
  deriving (Eq, Show, FromJSON)

newtype Script =
  Script
    { unScript :: Text
    }
  deriving (Eq, Show)

data Executable
  = ExecutableProgram (Path Rel File) Args
  | ExecutableScript Shell Script
  deriving (Eq, Show)

data Command
  = CommandArgs (Vector String)
  | CommandScript Shell Script
  deriving (Eq, Show)

instance FromJSON Command where
  parseJSON (Object v) = do
    specifiedShell <- v .:? "shell"
    shell <- maybe defaultShell return specifiedShell
    script <- v .: "script"
    return $ CommandScript shell (Script script)
  parseJSON (String script) = do
    shell <- defaultShell
    return $ CommandScript shell (Script script)
  parseJSON (Array args) =
    if Vector.null args
      then typeMismatch "shell" (Array args)
      else CommandArgs <$> sequence (parseJSON <$> args)
  parseJSON invalid = typeMismatch "command" invalid

newtype Status =
  Status
    { unStatus :: Int
    }
  deriving (Eq, Show)

instance Default Status where
  def = Status 0

newtype StdIn =
  StdIn
    { unStdIn :: Text
    }
  deriving (Eq, Show)

instance Default StdIn where
  def = StdIn Text.empty

newtype StdOut =
  StdOut
    { unStdOut :: Text
    }
  deriving (Eq, Show)

instance Default StdOut where
  def = StdOut Text.empty

newtype StdErr =
  StdErr
    { unStdErr :: Text
    }
  deriving (Eq, Show)

instance Default StdErr where
  def = StdErr Text.empty

newtype FixtureName =
  FixtureName
    { unFixtureName :: String
    }
  deriving (Eq, Show, IsString)
