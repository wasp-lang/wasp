{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.LSP.ServerM
  ( RLspM,
    ServerM,
    HandlerM,
    handler,
    runRLspM,
    logM,
    sendReactorInput,
    modify,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, readTVarIO, writeTChan)
import Control.Lens ((^.))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT), asks, runReaderT)
import Control.Monad.Trans (MonadIO (liftIO))
import Language.LSP.Server (LspM, MonadLsp)
import qualified System.Log.Logger as L
import Wasp.LSP.Reactor (ReactorInput)
import Wasp.LSP.ServerConfig (ServerConfig)
import Wasp.LSP.ServerState (ServerState, reactorIn)

-- | \"Reader LSP monad\": The LSP monad with a 'ReaderT' for extra state. Use
-- the type aliases 'ServerM' and 'HandlerM' instead of using this type directly.
newtype RLspM s a = RLspM
  { unServerM :: ReaderT s (LspM ServerConfig) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader s,
      MonadIO,
      MonadUnliftIO,
      MonadLsp ServerConfig
    )

-- | 'RLspM' specialized to @'TVar' 'ServerState'@. This is how you can modify
-- the server state.
--
-- We use a reader with a 'TVar' instead of a state monad because we want to
-- be able to modify the state from other threads.
type ServerM = RLspM (TVar ServerState)

-- | Most LSP handlers should use this instead of 'ServerM', as there are only
-- limited places where modifying the state is needed.
type HandlerM = RLspM ServerState

-- | Run a 'HandlerM' in 'ServerM'.
handler :: HandlerM a -> ServerM a
handler act = RLspM $
  ReaderT $ \stateTVar -> do
    state <- liftIO $ readTVarIO stateTVar
    runRLspM state act

-- | Modify the state inside the 'TVar' in the reader context.
modify :: (ServerState -> ServerState) -> ServerM ()
modify f = do
  stateTVar <- ask
  liftIO $ atomically $ modifyTVar stateTVar f

runRLspM ::
  s ->
  RLspM s a ->
  LspM ServerConfig a
runRLspM state m = runReaderT (unServerM m) state

-- | Send an action to the reactor thread.
sendReactorInput :: (MonadReader ServerState m, MonadIO m) => ReactorInput -> m ()
sendReactorInput inp = do
  rin <- asks (^. reactorIn)
  liftIO $ atomically $ writeTChan rin inp

-- | Log a string.
--
-- Behavior depends on the "--log" command line flag. If set to "[OUTPUT]",
-- logged messages will be displayed in the LSP client (e.g. for VSCode, in the
-- "Wasp Language Extension" output panel). Otherwise, it may be sent to a file
-- or not recorded at all.
instance MonadLog (RLspM s) where
  logM = liftIO . L.logM "haskell-lsp" L.DEBUG
