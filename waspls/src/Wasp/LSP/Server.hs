{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Wasp.LSP.Server
  ( serve,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson as Aeson
import Data.Default (Default (def))
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import System.Exit (ExitCode (ExitFailure), exitWith)
import qualified System.Log.Logger
import Wasp.LSP.Core (ServerConfig, ServerError (ServerError), ServerM, ServerState, Severity (..))
import Wasp.LSP.Handlers

serve :: Maybe FilePath -> IO ()
serve maybeLogFile = do
  setupLspLogger maybeLogFile

  let defaultServerState = def :: ServerState
  state <- MVar.newMVar defaultServerState

  let lspServerInterpretHandler env =
        LSP.Iso {forward = runHandler, backward = liftIO}
        where
          runHandler :: ServerM a -> IO a
          runHandler handler =
            -- Get the state from the "MVar", run the handler in IO and update
            -- the "MVar" state with the end state of the handler.
            MVar.modifyMVar state \oldState -> do
              LSP.runLspT env do
                (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                result <- case e of
                  Left (ServerError severity errMessage) -> sendErrorMessage severity errMessage
                  Right a -> return a

                return (newState, result)

  exitCode <-
    LSP.runServer
      LSP.ServerDefinition
        { defaultConfig = def :: ServerConfig,
          onConfigurationChange = lspServerUpdateConfig,
          doInitialize = lspServerDoInitialize,
          staticHandlers = lspServerHandlers,
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
      LSP.completionTriggerCharacters = Just [':']
    }

lspServerHandlers :: LSP.Handlers ServerM
lspServerHandlers =
  mconcat
    [ initializedHandler,
      didOpenHandler,
      didSaveHandler,
      didChangeHandler
    ]

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

-- | Send an error message to the LSP client.
--
-- Sends "Severity.Log" level errors to the output panel. Higher severity errors
-- are displayed in the window (i.e. in VSCode as a toast notification in the
-- bottom right).
sendErrorMessage :: Severity -> Text.Text -> LSP.LspT ServerConfig IO a
sendErrorMessage Log errMessage = do
  let messageType = LSP.MtLog

  LSP.sendNotification LSP.SWindowLogMessage $
    LSP.LogMessageParams {_xtype = messageType, _message = errMessage}
  liftIO (fail (Text.unpack errMessage))
sendErrorMessage severity errMessage = do
  let messageType = case severity of
        Error -> LSP.MtError
        Warning -> LSP.MtWarning
        Info -> LSP.MtInfo
        Log -> LSP.MtLog

  LSP.sendNotification LSP.SWindowShowMessage $
    LSP.ShowMessageParams {_xtype = messageType, _message = errMessage}
  liftIO (fail (Text.unpack errMessage))
