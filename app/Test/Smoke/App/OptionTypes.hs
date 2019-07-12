module Test.Smoke.App.OptionTypes
  ( AppOptions(..)
  , ColorOutput(..)
  , Mode(..)
  ) where

import Test.Smoke (Options(..))
import Test.Smoke.App.Diff.Types
import Test.Smoke.App.OptionColorOutput

data AppOptions =
  AppOptions
    { optionsExecution :: Options
    , optionsColor :: ColorOutput
    , optionsMode :: Mode
    , optionsDiffEngine :: DiffEngine
    }

data Mode
  = Check
  | Bless
