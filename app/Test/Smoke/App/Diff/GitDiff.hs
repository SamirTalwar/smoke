module Test.Smoke.App.Diff.GitDiff
  ( engine
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteStringChar
import qualified Data.List.NonEmpty as NonEmpty
import Test.Smoke.App.Diff.ExternalDiffCommand
import Test.Smoke.App.Diff.Types
import Test.Smoke.App.OptionColorOutput

engine :: DiffEngine
engine =
  DiffEngine
    { engineName = name
    , engineEnabled = enabled executable
    , engineRender =
        \color left right -> dropHeader <$> render (command color) left right
    }

name :: String
name = "git"

command :: ColorOutput -> Command
command color =
  NonEmpty.fromList $ [executable, "diff", "--no-index"] ++ colorFlags
  where
    colorFlags =
      case color of
        Color -> ["--color"]
        NoColor -> []

executable :: String
executable = "git"

dropHeader :: ByteString -> ByteString
dropHeader = ByteStringChar.unlines . drop 4 . ByteStringChar.lines
