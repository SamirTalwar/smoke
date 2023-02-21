module Test.Smoke.Shell where

import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Vector as Vector
import Test.Smoke.Paths
import Test.Smoke.Types

defaultShellScriptName :: String
defaultShellScriptName = "smoke.bat"

defaultShell :: ExceptT PathError IO Shell
defaultShell = do
  cmd <- findExecutable $ parseFile "cmd"
  let args = Args (Vector.fromList ["/q", "/c"])
  pure $ Shell cmd args
