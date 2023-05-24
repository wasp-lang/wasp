module Control.Monad.Log.Class
  ( MonadLog (logM),
  )
where

class Monad m => MonadLog m where
  -- | Write a message to the log
  logM :: String -> m ()
