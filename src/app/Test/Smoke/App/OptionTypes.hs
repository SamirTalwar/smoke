module Test.Smoke.App.OptionTypes
  ( InitOptions (..),
    AppOptions (..),
    ColorOutput (..),
    ShowSuiteNames,
  )
where

import Test.Smoke (Mode, Options)
import Test.Smoke.App.Diff.Types
import Test.Smoke.App.OptionColorOutput

data InitOptions
  = InitAppOptions AppOptions
  | ShowVersionText

data AppOptions = AppOptions
  { optionsExecution :: Options,
    optionsColor :: ColorOutput,
    optionsMode :: Mode,
    optionsDiffEngine :: DiffEngine
  }

type ShowSuiteNames = Bool
