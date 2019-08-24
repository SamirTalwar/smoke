module Test.Smoke.Shell where

import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Base
import Test.Smoke.Types.Errors
import Test.Smoke.Types.Executable

defaultShell :: ExceptT SmokeExecutableError IO Shell
defaultShell = do
  sh <- findExecutable "sh"
  return $ Shell sh mempty

defaultShellExecute :: Vector String
defaultShellExecute = Vector.fromList ["sh", "-c"]

shellFromCommandLine :: CommandLine -> ExceptT SmokeExecutableError IO Shell
shellFromCommandLine (CommandLine shellName shellArgs) = do
  shellCommand <- findExecutable shellName
  return $ Shell shellCommand shellArgs
