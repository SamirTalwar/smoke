{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Smoke.Types.Filters where

import Data.Aeson
import Test.Smoke.Types.Base

newtype Filter = Filter Command
  deriving (FromJSON)
