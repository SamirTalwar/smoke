module Test.Smoke.Types.Plans where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Executable
import Test.Smoke.Types.Files
import Test.Smoke.Types.Tests

newtype Plan
  = Plan [SuitePlan]
  deriving (Eq, Show)

data SuitePlan
  = SuitePlanError SuiteName SuiteError
  | SuitePlan SuiteName (ResolvedPath Dir) [TestPlanOutcome]
  deriving (Eq, Show)

data TestPlanOutcome
  = TestPlanError Test SmokePlanningError
  | TestPlanSuccess TestPlan
  deriving (Eq, Show)

data TestPlan = TestPlan
  { planTest :: Test,
    planWorkingDirectory :: WorkingDirectory,
    planShell :: Maybe Shell,
    planExecutable :: Executable,
    planArgs :: Args,
    planStdIn :: StdIn,
    planStatus :: Assert Status,
    planStdOut :: Vector (Assert StdOut),
    planStdErr :: Vector (Assert StdErr),
    planFiles :: Map (RelativePath File) (Vector (Assert TestFileContents)),
    planRevert :: Vector (ResolvedPath Dir)
  }
  deriving (Eq, Show)
