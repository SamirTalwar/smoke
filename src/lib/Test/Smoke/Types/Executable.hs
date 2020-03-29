module Test.Smoke.Types.Executable where

import Test.Smoke.Paths
import Test.Smoke.Types.Base

data Shell
  = Shell (ResolvedPath File) Args
  deriving (Eq, Show)

data Executable
  = ExecutableProgram (ResolvedPath File) Args
  | ExecutableScript Shell Script
  deriving (Eq, Show)
