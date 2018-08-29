{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Smoke.Types.Base where

import Data.Aeson
import Data.Text (Text)

type SuiteName = String

type TestName = String

type Contents = Text

newtype Executable = Executable
  { unExecutable :: String
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

newtype StdIn = StdIn
  { unStdIn :: Contents
  } deriving (Eq, Show)

newtype StdOut = StdOut
  { unStdOut :: Contents
  } deriving (Eq, Show)

newtype StdErr = StdErr
  { unStdErr :: Contents
  } deriving (Eq, Show)
