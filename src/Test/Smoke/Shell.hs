module Test.Smoke.Shell where

import Control.Monad.Fail (MonadFail)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Executable

defaultShell :: MonadFail m => m Shell
defaultShell = do
  sh <- parseAbsOrRelFile "sh"
  return $ Shell sh mempty

defaultShellExecute :: Vector String
defaultShellExecute = Vector.fromList ["sh", "-c"]
