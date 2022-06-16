{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Wasp.LSP.Server
  ( run,
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
import qualified Language.LSP.Types as J
import System.Exit (ExitCode (ExitFailure), exitWith)
import qualified System.Log.Logger
import Wasp.LSP.Core (ServerConfig, ServerM, Severity (..), State)
import Wasp.LSP.Handlers

run :: Maybe FilePath -> IO ()
run maybeLogFile = do
  setupLspLogger maybeLogFile

  state <- MVar.newMVar (def :: State)

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
                  Left (severity, errMessage) -> sendErrorMessage severity errMessage
                  Right a -> return a

                return (newState, result)

  exitCode <-
    LSP.runServer
      LSP.ServerDefinition
        { defaultConfig = def :: ServerConfig,
          onConfigurationChange = lspServerOnConfigChange,
          doInitialize = lspServerDoInitialize,
          staticHandlers = lspServerHandlers,
          interpretHandler = lspServerInterpretHandler,
          options = lspServerOptions
        }

  case exitCode of
    0 -> return ()
    n -> exitWith (ExitFailure n)

-- | Setup DEBUG logger. Logs at other levels are ignored.
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

lspServerOnConfigChange :: ServerConfig -> Aeson.Value -> Either Text.Text ServerConfig
lspServerOnConfigChange _oldConfig json =
  case Aeson.fromJSON json of
    Aeson.Success config -> Right config
    Aeson.Error string -> Left (Text.pack string)

lspServerDoInitialize ::
  LSP.LanguageContextEnv ServerConfig ->
  J.Message 'J.Initialize ->
  IO (Either J.ResponseError (LSP.LanguageContextEnv ServerConfig))
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
syncOptions :: J.TextDocumentSyncOptions
syncOptions =
  J.TextDocumentSyncOptions
    { -- Send open/close notifications be sent to the server.
      _openClose = Just True,
      -- Keep a copy of text documents contents in the VFS. When the document is
      -- changed, only send the updates instead of the entire contents.
      _change = Just J.TdSyncIncremental,
      -- Don't send will-save notifications to the server.
      _willSave = Just False,
      -- Don't send will-save-wait-until notifications to the server.
      _willSaveWaitUntil = Just False,
      -- Don't send save notifications to the server.
      _save = Just (J.InR (J.SaveOptions (Just True)))
    }

-- | Send an error message to the LSP client.
--
-- Sends "Severiy.Log" level errors to the output panel. Higher severity errors
-- are displayed in the window (i.e. in VSCode as a toast notification in the
-- bottom right).
sendErrorMessage :: Severity -> Text.Text -> LSP.LspT ServerConfig IO a
sendErrorMessage Log errMessage = do
  let messageType = J.MtLog

  LSP.sendNotification J.SWindowLogMessage $
    J.LogMessageParams {_xtype = messageType, _message = errMessage}
  liftIO (fail (Text.unpack errMessage))
sendErrorMessage severity errMessage = do
  let messageType = case severity of
        Error -> J.MtError
        Warning -> J.MtWarning
        Info -> J.MtInfo
        Log -> J.MtLog

  LSP.sendNotification J.SWindowShowMessage $
    J.ShowMessageParams {_xtype = messageType, _message = errMessage}
  liftIO (fail (Text.unpack errMessage))
