module Test.Smoke.App.Diff.Types
  ( DiffEngine(..)
  , RenderDiff
  ) where

import Data.ByteString (ByteString)

data DiffEngine = DiffEngine
  { engineName :: String
  , engineRender :: RenderDiff
  }

type RenderDiff = ByteString -> ByteString -> ByteString
