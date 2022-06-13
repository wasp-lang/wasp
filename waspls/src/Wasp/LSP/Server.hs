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
import Wasp.LSP.Handlers
import Wasp.LSP.State (HandlerM, ServerConfig, Severity (..), State)

run :: Maybe FilePath -> IO ()
run maybeLogFile = do
  -- Setup DEBUG logger. Logs at other levels are ignored.
  case maybeLogFile of
    Nothing ->
      -- Don't set up any logger: logs are not output anywhere
      pure ()
    Just "[OUTPUT]" ->
      -- Send log messages at "DEBUG" level to the LSP client
      LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
    file ->
      -- Send log messages at "DEBUG" level to the file path given
      LSP.setupLogger file [] System.Log.Logger.DEBUG

  state <- MVar.newMVar (def :: State)

  let lspServerInterpretHandler env =
        LSP.Iso {forward = runHandler, backward = liftIO}
        where
          runHandler :: HandlerM a -> IO a
          runHandler handler =
            MVar.modifyMVar state \oldState -> do
              LSP.runLspT env do
                (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                result <- case e of
                  Left (Log, _message) -> do
                    let _xtype = J.MtLog

                    LSP.sendNotification J.SWindowLogMessage $
                      J.LogMessageParams {_xtype = _xtype, _message = _message}
                    liftIO (fail (Text.unpack _message))
                  Left (severity_, _message) -> do
                    let _xtype = case severity_ of
                          Error -> J.MtError
                          Warning -> J.MtWarning
                          Info -> J.MtInfo
                          Log -> J.MtLog

                    LSP.sendNotification J.SWindowShowMessage $
                      J.ShowMessageParams {_xtype = _xtype, _message = _message}
                    liftIO (fail (Text.unpack _message))
                  Right a -> do
                    return a

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

lspServerHandlers :: LSP.Handlers HandlerM
lspServerHandlers =
  mconcat
    [ initializedHandler,
      didOpenHandler,
      didSaveHandler,
      didChangeHandler
    ]

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
