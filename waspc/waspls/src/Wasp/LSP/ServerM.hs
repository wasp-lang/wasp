module Wasp.LSP.ServerM
  ( ServerM,
    ServerError (..),
    Severity (..),
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
    logM,
    lift,
    catchError,
    throwError,
  )
where

import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as StateT
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Data.Text (Text)
import Language.LSP.Server (LspT)
import qualified System.Log.Logger as L
import Wasp.LSP.ServerConfig (ServerConfig)
import Wasp.LSP.ServerState (ServerState)

type ServerM = ExceptT ServerError (StateT ServerState (LspT ServerConfig IO))

-- | Log a string.
--
-- Behavior depends on the "--log" command line flag. If set to "[OUTPUT]",
-- logged messages will be displayed in the LSP client (e.g. for VSCode, in the
-- "Wasp Language Extension" output panel). Otherwise, it may be sent to a file
-- or not recorded at all.
logM :: String -> ServerM ()
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
