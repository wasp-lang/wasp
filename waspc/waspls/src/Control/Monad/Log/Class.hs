module Control.Monad.Log.Class
  ( MonadLog (logM),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (lift)

class Monad m => MonadLog m where
  -- | Write a message to the log
  logM :: String -> m ()

instance MonadLog m => MonadLog (ExceptT e m) where
  logM = lift . logM

instance MonadLog m => MonadLog (StateT s m) where
  logM = lift . logM

instance MonadLog m => MonadLog (ReaderT r m) where
  logM = lift . logM
