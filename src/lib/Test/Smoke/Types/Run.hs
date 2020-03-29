module Test.Smoke.Types.Run where

import Data.Vector (Vector)
import Test.Smoke.Types.Base

data Options
  = Options
      { optionsCommand :: Maybe Command,
        optionsTestLocations :: Vector String
      }
  deriving (Eq, Show)

data Mode
  = Check
  | Bless
  deriving (Eq, Show)
