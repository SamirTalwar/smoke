module Test.Smoke.Types.Base where

import Data.Text (Text)

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
