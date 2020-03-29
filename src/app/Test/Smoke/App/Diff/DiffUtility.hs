module Test.Smoke.App.Diff.DiffUtility
  ( engine,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Test.Smoke.App.Diff.ExternalDiffCommand
import Test.Smoke.App.Diff.Types

engine :: DiffEngine
engine =
  DiffEngine
    { engineName = name,
      engineEnabled = enabled executable,
      engineRender = \_ -> render command
    }

name :: String
name = "diff"

command :: Command
command = NonEmpty.fromList [executable]

executable :: String
executable = "diff"
