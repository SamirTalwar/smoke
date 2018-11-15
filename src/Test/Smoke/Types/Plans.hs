module Test.Smoke.Types.Plans where

import Data.Vector (Vector)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Tests

newtype Plan =
  Plan [(SuiteName, Either SmokeDiscoveryError [Either TestPlanError TestPlan])]
  deriving (Eq, Show)

data TestPlan = TestPlan
  { planTest :: Test
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
