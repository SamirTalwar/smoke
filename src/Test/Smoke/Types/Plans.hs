module Test.Smoke.Types.Plans where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Path
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Executable
import Test.Smoke.Types.Files
import Test.Smoke.Types.Tests

newtype Plan =
  Plan [SuitePlan]
  deriving (Eq, Show)

data SuitePlan
  = SuitePlanError SuiteName SuiteError
  | SuitePlan SuiteName (Path Abs Dir) [TestPlanOutcome]
  deriving (Eq, Show)

data TestPlanOutcome
  = TestPlanError Test SmokePlanningError
  | TestPlanSuccess TestPlan
  deriving (Eq, Show)

data TestPlan =
  TestPlan
    { planTest :: Test
    , planWorkingDirectory :: WorkingDirectory
    , planShell :: Maybe Shell
    , planExecutable :: Executable
    , planArgs :: Args
    , planStdIn :: StdIn
    , planStatus :: Status
    , planStdOut :: Vector StdOut
    , planStdErr :: Vector StdErr
    , planFiles :: Map (Path Rel File) (Vector TestFileContents)
    , planRevert :: Vector (Path Abs Dir)
    }
  deriving (Eq, Show)
