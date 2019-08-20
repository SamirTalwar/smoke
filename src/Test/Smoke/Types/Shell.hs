module Test.Smoke.Types.Shell where

import Control.Monad.Catch.Pure (runCatchT)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as Vector
import Path
import Test.Smoke.Paths
import Test.Smoke.Types.Args

data Shell =
  Shell (Path Rel File) Args
  deriving (Eq, Show)

instance FromJSON Shell where
  parseJSON =
    withArray "shell" $ \values ->
      if Vector.null values
        then typeMismatch "shell" (Array values)
        else do
          strings <- Vector.mapM parseJSON values
          shellCommand <-
            either (fail . show) return =<<
            runCatchT (parseAbsOrRelFile (Vector.head strings))
          let shellArgs = Args $ Vector.tail strings
          return $ Shell shellCommand shellArgs
