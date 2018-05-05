module Test.Smoke.App.Diff
  ( Renderer
  , renderers
  , findRenderer
  , getRenderer
  ) where

import qualified Test.Smoke.App.Diff.Native as Native
import Test.Smoke.App.Diff.Types

type Renderer = DiffRenderer

renderers :: [String]
renderers = ["native"]

findRenderer :: IO Renderer
findRenderer = return Native.render

getRenderer :: String -> Maybe Renderer
getRenderer "native" = Just Native.render
getRenderer _ = Nothing
