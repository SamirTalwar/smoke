module Test.Smoke.Shell where

import Control.Monad.Trans.Except (ExceptT)
import Test.Smoke.Paths
import Test.Smoke.Types

defaultShellScriptName :: String
defaultShellScriptName = "smoke.sh"

defaultShell :: ExceptT PathError IO Shell
defaultShell = do
  sh <- findExecutable $ parseFile "sh"
  return $ Shell sh mempty
