module Test.Smoke.App.Diff.Types
  ( DiffEngine(..)
  , RenderDiff
  ) where

import Data.ByteString (ByteString)

data DiffEngine = DiffEngine
  { engineName :: String
  , engineEnabled :: IO Bool
  , engineRender :: RenderDiff
  }

type RenderDiff = ByteString -> ByteString -> IO ByteString
