module Test.Smoke.Errors where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Control.Monad.Catch.Pure (CatchT, runCatchT)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

runExceptTIO :: (Exception e, MonadIO m) => ExceptT e m a -> m a
runExceptTIO = either (liftIO . throwIO) return <=< runExceptT

catchAndFail :: MonadFail m => CatchT m a -> m a
catchAndFail = either (fail . show) return <=< runCatchT

onNothingThrow :: Monad m => e -> Maybe a -> ExceptT e m a
onNothingThrow exception = maybe (throwE exception) return

onNothingThrow_ :: Monad m => e -> Maybe a -> ExceptT e m ()
onNothingThrow_ exception = (>> return ()) . onNothingThrow exception

handleError :: (a -> b) -> Either a b -> b
handleError handler = either handler id
