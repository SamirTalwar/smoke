{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Smoke.Types.Filters where

import Data.Aeson
import Test.Smoke.Types.Base

newtype FixtureFilter =
  FixtureFilter Command
  deriving (Eq, Show, FromJSON)

data Filtered a
  = Unfiltered a
  | Filtered a FixtureFilter
  deriving (Eq, Show)

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
