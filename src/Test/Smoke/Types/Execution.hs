module Test.Smoke.Types.Execution where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Test.Smoke.Types.Errors

type Execution = ExceptT TestErrorMessage IO

onNothingThrow :: Monad m => e -> Maybe a -> ExceptT e m a
onNothingThrow exception = maybe (throwE exception) return

onNothingThrow_ :: Monad m => e -> Maybe a -> ExceptT e m ()
onNothingThrow_ exception = maybe (throwE exception) (const $ return ())

handleError :: (a -> b) -> Either a b -> b
handleError handler = either handler id
