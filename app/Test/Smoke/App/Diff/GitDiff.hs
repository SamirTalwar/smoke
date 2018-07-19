module Test.Smoke.App.Diff.GitDiff
  ( engine
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteStringChar
import qualified Data.List.NonEmpty as NonEmpty
import Test.Smoke.App.Diff.ExternalDiffCommand
import Test.Smoke.App.Diff.Types

engine :: DiffEngine
engine =
  DiffEngine
    { engineName = name
    , engineEnabled = enabled command
    , engineRender = \left right -> dropHeader <$> render command left right
    }

name :: String
name = "git"

command :: Command
command = NonEmpty.fromList ["git", "diff", "--no-index"]

dropHeader :: ByteString -> ByteString
dropHeader = ByteStringChar.unlines . drop 4 . ByteStringChar.lines
