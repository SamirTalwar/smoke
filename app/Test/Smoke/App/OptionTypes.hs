module Test.Smoke.App.OptionTypes
  ( AppOptions(..)
  , ColorOutput(..)
  , Mode(..)
  ) where

import Test.Smoke (Options(..))
import Test.Smoke.App.Diff.Types

data AppOptions = AppOptions
  { optionsExecution :: Options
  , optionsColor :: ColorOutput
  , optionsMode :: Mode
  , optionsDiffEngine :: DiffEngine
  }

data ColorOutput
  = Color
  | NoColor
  deriving (Eq)

data Mode
  = Check
  | Bless
