module Test.Smoke.Types.Executable where

import Path
import Test.Smoke.Types.Base

data Shell =
  Shell (Path Rel File) Args
  deriving (Eq, Show)

data Executable
  = ExecutableProgram (Path Rel File) Args
  | ExecutableScript Shell Script
  deriving (Eq, Show)
