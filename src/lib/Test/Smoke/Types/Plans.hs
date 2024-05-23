module Test.Smoke.Types.Plans where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Assert
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Executable
import Test.Smoke.Types.Tests

newtype Plan
  = Plan [SuitePlan]

data SuitePlan
  = SuitePlanError SuiteName SuiteError
  | SuitePlan SuiteName (Path Resolved Dir) [TestPlanOutcome]

data TestPlanOutcome
  = TestPlanError Test SmokePlanningError
  | TestPlanSuccess TestPlan

data TestPlan = TestPlan
  { planTest :: Test,
    planWorkingDirectory :: WorkingDirectory,
    planShell :: Maybe Shell,
    planExecutable :: Executable,
    planArgs :: Args,
    planEnvironment :: Maybe EnvVars,
    planStdIn :: StdIn,
    planStatus :: Status,
    planStdOut :: Vector (Assert StdOut),
    planStdErr :: Vector (Assert StdErr),
    planFiles :: Map (Path Relative File) (Vector (Assert TestFileContents)),
    planRevert :: Vector (Path Resolved Dir)
  }
