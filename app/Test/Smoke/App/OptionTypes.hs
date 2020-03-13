module Test.Smoke.App.OptionTypes
  ( AppOptions (..),
    ColorOutput (..),
    ShowSuiteNames,
  )
where

import Test.Smoke (Mode, Options)
import Test.Smoke.App.Diff.Types
import Test.Smoke.App.OptionColorOutput

data AppOptions
  = AppOptions
      { optionsExecution :: Options,
        optionsColor :: ColorOutput,
        optionsMode :: Mode,
        optionsDiffEngine :: DiffEngine
      }

type ShowSuiteNames = Bool
