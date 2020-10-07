{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Test.Smoke.Types.Assert where

import Data.Default (Default (..))
import Data.Vector (Vector)
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Filters

data Assert a where
  AssertEquals :: Eq a => a -> Assert a
  AssertContains :: FixtureType a => a -> Assert a
  AssertFiltered :: FixtureType a => Filter -> Assert a -> Assert a
  AssertFileError :: SmokeFileError -> Assert a

instance (Default a, Eq a) => Default (Assert a) where
  def = AssertEquals def

newtype Expected a = Expected a
  deriving (Functor)

newtype Actual a = Actual a
  deriving (Functor)

data AssertionFailure a
  = AssertionFailureDiff (Expected a) (Actual a)
  | AssertionFailureContains (Expected a) (Actual a)
  | AssertionFailureExpectedFileError SmokeFileError (Actual a)
  | AssertionFailureActualFileError SmokeFileError
  deriving (Functor)

data AssertionFailures a
  = SingleAssertionFailure (AssertionFailure a)
  | MultipleAssertionFailures (Vector (AssertionFailure a))
  deriving (Functor)
