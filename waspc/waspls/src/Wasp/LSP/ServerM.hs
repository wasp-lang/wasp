{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.LSP.ServerM
  ( ServerM,
    runServerM,
    ServerError (..),
    Severity (..),
    liftLSP,
    logM,
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
  )
where

import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Control.Monad.State.Strict as StateT
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Data.Text (Text)
import Language.LSP.Server (LspT)
import qualified System.Log.Logger as L
import Wasp.LSP.ServerConfig (ServerConfig)
import Wasp.LSP.ServerState (ServerState)

newtype ServerM a = ServerM
  { unServerM :: ExceptT ServerError (StateT ServerState (LspT ServerConfig IO)) a
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
