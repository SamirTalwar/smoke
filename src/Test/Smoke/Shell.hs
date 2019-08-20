module Test.Smoke.Shell where

import Control.Monad.Catch.Pure (runCatchT)
import Control.Monad.Fail (MonadFail)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Test.Smoke.Paths
import Test.Smoke.Types.Shell

defaultShell :: MonadFail m => m Shell
defaultShell = do
  eitherSh <- runCatchT $ parseAbsOrRelFile "sh"
  sh <- either (fail . show) return eitherSh
  return $ Shell sh mempty

defaultShellExecute :: Vector String
defaultShellExecute = Vector.fromList ["sh", "-c"]
