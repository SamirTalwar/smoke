module Test.Smoke.App.Diff.Types
  ( DiffRenderer
  ) where

import Data.ByteString (ByteString)

type DiffRenderer = ByteString -> ByteString -> ByteString
