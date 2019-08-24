module Test.Smoke.Types.Executable where

import Path
import Test.Smoke.Types.Base

data Shell =
  Shell (Path Abs File) Args
  deriving (Eq, Show)

data Executable
  = ExecutableProgram (Path Abs File) Args
  | ExecutableScript Shell Script
  deriving (Eq, Show)
