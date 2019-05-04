{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Base where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Path

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

newtype SuiteName = SuiteName
  { unSuiteName :: String
  } deriving (Eq, Ord, Show)

newtype TestName = TestName
  { unTestName :: String
  } deriving (Eq, Ord, Show)

newtype WorkingDirectory = WorkingDirectory
  { unWorkingDirectory :: Path Abs Dir
  } deriving (Eq, Show, FromJSON)

newtype Executable = Executable
  { unExecutable :: Path Rel File
  } deriving (Eq, Show, FromJSON)

newtype Command = Command
  { unCommand :: [String]
  } deriving (Eq, Show, FromJSON)

newtype Args = Args
  { unArgs :: [String]
  } deriving (Eq, Show, FromJSON)

newtype Status = Status
  { unStatus :: Int
  } deriving (Eq, Show)

instance Default Status where
  def = Status 0

newtype StdIn = StdIn
  { unStdIn :: Text
  } deriving (Eq, Show)

instance Default StdIn where
  def = StdIn Text.empty

newtype StdOut = StdOut
  { unStdOut :: Text
  } deriving (Eq, Show)

instance Default StdOut where
  def = StdOut Text.empty

newtype StdErr = StdErr
  { unStdErr :: Text
  } deriving (Eq, Show)

instance Default StdErr where
  def = StdErr Text.empty
