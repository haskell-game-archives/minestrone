module Control.Monad.SurfaceT where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import SDL

-- ReaderT <Environment> <Monad> <Return Type>
type SurfaceT m a = ReaderT Surface m a

test :: MonadIO m => SurfaceT m ()
test = do
  surface <- ask
  liftT $ void $ surfaceBlit surface Nothing surface Nothing

liftT :: (MonadIO m, MonadTrans r) => IO a -> r m a
liftT = lift . liftIO
