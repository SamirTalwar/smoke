{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Test.Smoke.Types.Assert where

import Data.Default
import Test.Smoke.Types.Filters

data Assert a where
  AssertEqual :: Eq a => a -> Assert a
  AssertFiltered :: Filter -> Assert a -> Assert a

instance (Default a, Eq a) => Default (Assert a) where
  def = AssertEqual def

data AssertFailure a
  = AssertFailureDiff a a
  deriving (Functor)

assertFailureActual :: AssertFailure a -> a
assertFailureActual (AssertFailureDiff _ actual) = actual
