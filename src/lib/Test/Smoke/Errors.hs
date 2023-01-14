module Test.Smoke.Errors where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

runExceptTIO :: (Exception e, MonadIO m) => ExceptT e m a -> m a
runExceptTIO = either (liftIO . throwIO) pure <=< runExceptT

onNothingThrow :: Monad m => e -> Maybe a -> ExceptT e m a
onNothingThrow exception = maybe (throwE exception) pure

onNothingThrow_ :: Monad m => e -> Maybe a -> ExceptT e m ()
onNothingThrow_ exception = (>> pure ()) . onNothingThrow exception

handleError :: (a -> b) -> Either a b -> b
handleError handler = either handler id
