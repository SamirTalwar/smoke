module Test.Smoke.App.Diff.Types
  ( DiffEngine(..)
  , PrintDiff
  , RenderDiff
  ) where

import Data.ByteString (ByteString)
import Test.Smoke.App.OptionColorOutput

data DiffEngine = DiffEngine
  { engineName :: String
  , engineEnabled :: IO Bool
  , engineRender :: PrintDiff
  }

type PrintDiff = ColorOutput -> RenderDiff

type RenderDiff = ByteString -> ByteString -> IO ByteString
