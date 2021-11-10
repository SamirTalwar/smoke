module Test.Smoke.Types.Executable where

import Test.Smoke.Paths
import Test.Smoke.Types.Base

data Shell
  = Shell (Path Resolved File) Args
  deriving (Eq, Show)

data Executable
  = ExecutableProgram (Path Resolved File) Args
  | ExecutableScript Shell Script
  deriving (Eq, Show)
