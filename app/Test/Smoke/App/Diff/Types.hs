module Test.Smoke.App.Diff.Types
  ( DiffEngine(..)
  , PrintDiff
  , RenderDiff
  , Text
  ) where

import Data.Text (Text)
import Test.Smoke.App.OptionColorOutput

data DiffEngine = DiffEngine
  { engineName :: String
  , engineEnabled :: IO Bool
  , engineRender :: PrintDiff
  }

type PrintDiff = ColorOutput -> RenderDiff

type RenderDiff = Text -> Text -> IO Text
