module Test.Smoke.Types.Filters where

import Test.Smoke.Types.Base

data Filtered a
  = Unfiltered a
  | Filtered a Command
  deriving (Eq, Show)

unfiltered :: Filtered a -> a
unfiltered (Unfiltered value) = value
unfiltered (Filtered unfilteredValue _) = unfilteredValue
