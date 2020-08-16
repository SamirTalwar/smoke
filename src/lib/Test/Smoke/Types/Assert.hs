{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Smoke.Types.Assert where

import Data.Default

data Assert a where
  AssertEqual :: Eq a => a -> Assert a

deriving instance Eq a => Eq (Assert a)

deriving instance Show a => Show (Assert a)

instance (Default a, Eq a) => Default (Assert a) where
  def = AssertEqual def

data AssertFailure a
  = AssertFailureDiff a a
  deriving (Eq, Functor, Show)

assertFailureActual :: AssertFailure a -> a
assertFailureActual (AssertFailureDiff _ actual) = actual
