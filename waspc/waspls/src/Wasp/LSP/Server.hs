{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Wasp.LSP.Server
  ( serve,
  )
where

import Control.Concurrent (newEmptyMVar, tryPutMVar)
import Control.Concurrent.STM (newTChanIO, newTVarIO)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Default (Default (def))
import qualified Data.HashMap.Strict as M
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import System.Exit (ExitCode (ExitFailure), exitWith)
import qualified System.Log.Logger
import qualified Wasp.LSP.Command as Command
import Wasp.LSP.Debouncer (newDebouncerIO)
import Wasp.LSP.Handlers
import Wasp.LSP.Reactor (startReactorThread)
import Wasp.LSP.ServerConfig (ServerConfig)
import Wasp.LSP.ServerMonads (ServerM, runRLspM)
import Wasp.LSP.ServerState
  ( RegistrationTokens (RegTokens, _watchSourceFilesToken),
    ServerState (ServerState, _cst, _currentWaspSource, _debouncer, _latestDiagnostics, _reactorIn, _regTokens, _tsExports, _waspFileUri),
  )
import Wasp.LSP.SignatureHelp (signatureHelpRetriggerCharacters, signatureHelpTriggerCharacters)

lspServerHandlers :: IO () -> LSP.Handlers ServerM
lspServerHandlers stopReactor =
  mconcat
    [ initializedHandler,
      shutdownHandler stopReactor,
      didOpenHandler,
      didSaveHandler,
      didChangeHandler,
      executeCommandHandler,
      completionHandler,
      signatureHelpHandler,
      gotoDefinitionHandler,
      codeActionHandler
    ]

serve :: Maybe FilePath -> IO ()
serve maybeLogFile = do
  setupLspLogger maybeLogFile

  -- Reactor setup
  reactorLifetime <- newEmptyMVar
  let stopReactor = void $ tryPutMVar reactorLifetime ()
  reactorIn <- newTChanIO
  startReactorThread reactorLifetime reactorIn

  -- Debouncer setup
  debouncer <- newDebouncerIO

  let defaultServerState =
        ServerState
          { _waspFileUri = Nothing,
            _currentWaspSource = "",
            _latestDiagnostics = [],
            _cst = Nothing,
            _tsExports = M.empty,
            _regTokens = RegTokens {_watchSourceFilesToken = Nothing},
            _reactorIn = reactorIn,
            _debouncer = debouncer
          }

  -- Create the TVar that manages the server state.
  stateTVar <- newTVarIO defaultServerState

  let lspServerInterpretHandler env =
        LSP.Iso {forward = runHandler, backward = liftIO}
        where
          runHandler :: ServerM a -> IO a
          runHandler handler =
            LSP.runLspT env $ runRLspM stateTVar handler

  exitCode <-
    LSP.runServer
      LSP.ServerDefinition
        { defaultConfig = def :: ServerConfig,
          onConfigurationChange = lspServerUpdateConfig,
          doInitialize = lspServerDoInitialize,
          staticHandlers = lspServerHandlers stopReactor,
          interpretHandler = lspServerInterpretHandler,
          options = lspServerOptions
        }

  case exitCode of
    0 -> return ()
    n -> exitWith (ExitFailure n)

-- | Setup global DEBUG logger. Logs at other levels are ignored.
--
-- Use 'System.Log.Logger.logM' at "DEBUG" level to write to this log.
--
-- @setupLspLogger Nothing@ doesn't set up any logger, so logs are not output
-- anywhere.
--
-- @setupLspLogger (Just "[OUTPUT]")@ sends log messages to the LSP client
--
-- @setupLspLogger (Just filepath)@ writes log messages to the path given
setupLspLogger :: Maybe FilePath -> IO ()
setupLspLogger Nothing = pure ()
setupLspLogger (Just "[OUTPUT]") = LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLspLogger file = LSP.setupLogger file [] System.Log.Logger.DEBUG

-- | Returns either a JSON parsing error message or the updated "ServerConfig".
lspServerUpdateConfig :: ServerConfig -> Aeson.Value -> Either Text.Text ServerConfig
lspServerUpdateConfig _oldConfig json =
  case Aeson.fromJSON json of
    Aeson.Success config -> Right config
    Aeson.Error string -> Left (Text.pack string)

lspServerDoInitialize ::
  LSP.LanguageContextEnv ServerConfig ->
  LSP.Message 'LSP.Initialize ->
  IO (Either LSP.ResponseError (LSP.LanguageContextEnv ServerConfig))
lspServerDoInitialize env _req = return (Right env)

lspServerOptions :: LSP.Options
lspServerOptions =
  (def :: LSP.Options)
    { LSP.textDocumentSync = Just syncOptions,
      LSP.completionTriggerCharacters = Just [':', ' '],
      LSP.signatureHelpTriggerCharacters = signatureHelpTriggerCharacters,
      LSP.signatureHelpRetriggerCharacters = signatureHelpRetriggerCharacters,
      LSP.executeCommandCommands = Just Command.availableCommands
    }

-- | Options to tell the client how to update the server about the state of text
-- documents in the workspace.
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { -- Send open/close notifications be sent to the server.
      _openClose = Just True,
      -- Keep a copy of text documents contents in the VFS. When the document is
      -- changed, only send the updates instead of the entire contents.
      _change = Just LSP.TdSyncIncremental,
      -- Don't send will-save notifications to the server.
      _willSave = Just False,
      -- Don't send will-save-wait-until notifications to the server.
      _willSaveWaitUntil = Just False,
      -- Send save notifications to the server.
      _save = Just (LSP.InR (LSP.SaveOptions (Just True)))
    }
