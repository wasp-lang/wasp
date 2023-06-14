{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.LSP.ServerM
  ( ServerM,
    runServerM,
    ServerError (..),
    Severity (..),
    liftLSP,
    logM,
    sendReactorInput,
    -- | You should usually use lenses for accessing the state.
    --
    -- __Examples:__
    --
    -- > import Control.Lens ((^.))
    -- > gets (^. diagnostics) -- Gets the list of diagnostics
    --
    -- > import Control.Lens ((.~))
    -- > modify (diagnostics .~ []) -- Clears diagnostics in the state
    StateT.gets,
    StateT.modify,
    lift,
    catchError,
    throwError,

    -- * ReaderM
    ReaderM (..),
    runReaderM,
  )
where

import Control.Concurrent.STM (atomically, writeTChan)
import Control.Lens ((^.))
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Control.Monad.State.Strict as StateT
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Data.Text (Text)
import Language.LSP.Server (LanguageContextEnv, LspM, LspT, MonadLsp, runLspT)
import qualified System.Log.Logger as L
import Wasp.LSP.Reactor (ReactorInput)
import Wasp.LSP.ServerConfig (ServerConfig)
import Wasp.LSP.ServerState (ServerState, reactorIn)

newtype ServerM a = ServerM
  { unServerM :: ExceptT ServerError (StateT ServerState (LspM ServerConfig)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError ServerError,
      MonadState ServerState,
      MonadIO
    )

runServerM ::
  ServerState ->
  ServerM a ->
  LspT ServerConfig IO (Either ServerError a, ServerState)
runServerM state m = runStateT (runExceptT $ unServerM m) state

-- | Run a LSP function in the "ServerM" monad.
liftLSP :: LspT ServerConfig IO a -> ServerM a
liftLSP m = ServerM $ lift $ lift m

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
instance MonadLog ServerM where
  logM = liftIO . L.logM "haskell-lsp" L.DEBUG

-- | The type for a language server error. These are separate from diagnostics
-- and should be reported when the server fails to process a request/notification
-- for some reason.
data ServerError = ServerError Severity Text

-- | Error severity levels
data Severity
  = -- | Displayed to user as an error
    Error
  | -- | Displayed to user as a warning
    Warning
  | -- | Displayed to user
    Info
  | -- | Not displayed to the user
    Log

-- | 'ServerM', but in a 'ReaderT' instead of 'StateT'.
--
-- NOTE: 'ReaderM' implements 'MonadLsp', but 'ServerM' does not. This is because
-- 'ServerM' has a monadic state, preventing it from being able to implement
-- 'MonadUnliftIO', which is required by 'MonadLsp'.
newtype ReaderM a = ReaderM {unReaderM :: ReaderT ServerState (LspM ServerConfig) a}
  deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadIO, MonadUnliftIO, MonadLsp ServerConfig)

instance MonadLog ReaderM where
  logM = liftIO . L.logM "haskell-lsp" L.DEBUG

runReaderM :: ReaderM a -> LanguageContextEnv ServerConfig -> ServerState -> IO a
runReaderM (ReaderM m) env state = runLspT env $ runReaderT m state
