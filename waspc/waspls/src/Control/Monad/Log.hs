{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Log
  ( LogT,
    Log,
    runLogT,
    runLog,
    MonadLog (logM),
  )
where

import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT, runWriterT)
import Data.Functor.Identity (Identity (runIdentity))

type Log = LogT Identity

runLog :: Log a -> (a, [String])
runLog m = runIdentity $ runLogT m

-- | Pure 'MonadLog' monad transformer.
newtype LogT m a = LogT (WriterT [String] m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runLogT :: LogT m a -> m (a, [String])
runLogT (LogT m) = runWriterT m

instance Monad m => MonadLog (LogT m) where
  logM msg = LogT $ tell [msg]
