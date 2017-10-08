module Test.Smoke.Types
  ( Command
  , Options(..)
  , Tests
  , Test(..)
  ) where

type Command = [String]

type Args = [String]

data Options = Options
  { optionsCommand :: Maybe Command
  , optionsColor :: Bool
  , optionsTestLocations :: [FilePath]
  } deriving (Eq, Show)

type Tests = [Test]

data Test = Test
  { testName :: String
  , testCommand :: Command
  , testArgs :: Maybe Args
  , testStdIn :: Maybe FilePath
  , testStdOut :: [FilePath]
  , testStdErr :: [FilePath]
  , testStatus :: Int
  } deriving (Eq, Show)
