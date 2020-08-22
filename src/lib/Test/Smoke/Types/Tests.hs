module Test.Smoke.Types.Tests where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Files
import Test.Smoke.Types.Fixtures

data TestSpecification
  = TestSpecification (Maybe Command) Suites

type Suites = [(SuiteName, Either SmokeDiscoveryError Suite)]

data Suite = Suite
  { suiteLocation :: ResolvedPath Dir,
    suiteWorkingDirectory :: Maybe WorkingDirectory,
    suiteShell :: Maybe CommandLine,
    suiteCommand :: Maybe Command,
    suiteTests :: [Test]
  }

data Test = Test
  { testName :: TestName,
    testIgnored :: Bool,
    testWorkingDirectory :: Maybe WorkingDirectory,
    testCommand :: Maybe Command,
    testArgs :: Maybe Args,
    testStdIn :: Maybe (Fixture StdIn),
    testStdOut :: Vector (Fixture StdOut),
    testStdErr :: Vector (Fixture StdErr),
    testStatus :: Fixture Status,
    testFiles :: Map (RelativePath File) (Vector (Fixture TestFileContents)),
    testRevert :: Vector (RelativePath Dir)
  }
