{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Smoke.Types.Args where

import Data.Aeson
import Data.Vector (Vector)

newtype Args =
  Args
    { unArgs :: Vector String
    }
  deriving (Eq, Show, Semigroup, Monoid, FromJSON)
