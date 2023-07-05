{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.LSP.ServerMonads
  ( -- * LSP Server Monads

    -- The state of the LSP server is used in two different ways:
    -- - Read only.
    -- - Read and write.
    --
    -- Additionally, the state is accessed from multiple threads concurrently.
    -- See waspls README for the architecture of the LSP server.
    --
    -- To facilitate this, there are two variants of the server monad: 'ServerM',
    -- with write-access to the shared state via a 'TVar', and 'HandlerM' for
    -- read-only access. In general, 'ServerM' should only be used in handlers
    -- that are doing analysis on source files, that is, computing syntactic
    -- and/or semantic information about the code that is needed for handlers to
    -- respond to LSP requests.
    --
    -- For example, processing a @textDocumentDidChange@ notification runs in
    -- 'ServerM' because it computes a new syntax tree for the wasp file,
    -- whereas a @textDocumentcompletion@ request handler runs in 'HandlerM',
    -- because it only needs to read from the latest analysis of the wasp file.
    --
    -- Under the hood, both monads are the 'RLspM' monad, distinguished only
    -- by whether the context type is 'TVar' or not.

    -- * Monads
    RLspM,
    ServerM,
    HandlerM,
    handler,
    runRLspM,

    -- * Operations
    sendToReactor,
    logM,
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
import qualified Language.LSP.Server as LSP
import qualified System.Log.Logger as L
import Wasp.LSP.Reactor (ReactorInput (ReactorAction))
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

-- | Send a 'ServerM' action to the reactor thread.
sendToReactor :: ServerM () -> ServerM ()
sendToReactor act = do
  stateTVar <- ask
  env <- LSP.getLspEnv
  rin <- handler $ asks (^. reactorIn)
  liftIO $ atomically $ writeTChan rin $ ReactorAction $ LSP.runLspT env $ runRLspM stateTVar act

runRLspM ::
  s ->
  RLspM s a ->
  LspM ServerConfig a
runRLspM state m = runReaderT (unServerM m) state

-- | Log a string.
--
-- Behavior depends on the "--log" command line flag. If set to "[OUTPUT]",
-- logged messages will be displayed in the LSP client (e.g. for VSCode, in the
-- "Wasp Language Extension" output panel). Otherwise, it may be sent to a file
-- or not recorded at all.
instance MonadLog (RLspM s) where
  logM = liftIO . L.logM "haskell-lsp" L.DEBUG
