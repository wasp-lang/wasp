{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}

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
import Language.LSP.Server (Options (..), ServerDefinition (..), type (<~>) (..))
import qualified Language.LSP.Server as LSP
import Language.LSP.Types (TextDocumentSyncOptions (..))
import qualified Language.LSP.Types as J
import System.Exit (ExitCode (ExitFailure), exitWith)
import qualified System.Log.Logger
import Wasp.LSP.Handlers
import Wasp.LSP.State (HandlerM, ServerConfig, Severity (..), State)

run :: Maybe FilePath -> IO ()
run maybeLogFile = do
  case maybeLogFile of
    Nothing -> pure ()
    Just "[OUTPUT]" -> LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
    file -> LSP.setupLogger file [] System.Log.Logger.DEBUG

  state <- MVar.newMVar (def :: State)

  let opts =
        def
          { textDocumentSync = Just syncOptions,
            completionTriggerCharacters = Just [':']
          }

  let initialize env _req = return (Right env)

  let onConfigChange _oldConfig json =
        case Aeson.fromJSON json of
          Aeson.Success config -> Right config
          Aeson.Error string -> Left (Text.pack string)

  let handlers =
        mconcat
          [ initializedHandler,
            didOpenHandler,
            didSaveHandler,
            didChangeHandler
          ]

  let interpret env = Iso {forward = runHandler, backward = liftIO}
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
      ServerDefinition
        { defaultConfig = def :: ServerConfig,
          onConfigurationChange = onConfigChange,
          doInitialize = initialize,
          staticHandlers = handlers,
          interpretHandler = interpret,
          options = opts
        }

  case exitCode of
    0 -> return ()
    n -> exitWith (ExitFailure n)

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just J.TdSyncIncremental,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just (J.InR (J.SaveOptions (Just False)))
    }
