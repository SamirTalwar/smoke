module Test.Smoke.Types.Plans where

import Data.Vector (Vector)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Paths
import Test.Smoke.Types.Tests

newtype Plan =
  Plan [SuitePlan]
  deriving (Eq, Show)

data SuitePlan
  = SuitePlanError { suitePlanErrorName :: SuiteName
                   , suitePlanError :: SmokeDiscoveryError }
  | SuitePlan { suitePlanName :: SuiteName
              , suitePlanLocation :: Path
              , suitePlanTests :: [Either TestPlanError TestPlan] }
  deriving (Eq, Show)

data TestPlan = TestPlan
  { planTest :: Test
  , planWorkingDirectory :: WorkingDirectory
  , planExecutable :: Executable
  , planArgs :: Args
  , planStdIn :: StdIn
  , planStatus :: Status
  , planStdOut :: Vector StdOut
  , planStdErr :: Vector StdErr
  } deriving (Eq, Show)

data TestPlanError =
  TestPlanError Test
                SmokePlanningError
  deriving (Eq, Show)
