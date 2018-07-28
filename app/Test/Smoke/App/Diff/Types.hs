module Test.Smoke.App.Diff.Types
  ( DiffEngine(..)
  , PrintDiff
  , RenderDiff
  ) where

import Test.Smoke (Contents)
import Test.Smoke.App.OptionColorOutput

data DiffEngine = DiffEngine
  { engineName :: String
  , engineEnabled :: IO Bool
  , engineRender :: PrintDiff
  }

type PrintDiff = ColorOutput -> RenderDiff

type RenderDiff = Contents -> Contents -> IO Contents
