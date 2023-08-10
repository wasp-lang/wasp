{-# LANGUAGE DataKinds #-}

module Wasp.LSP.Commands.Command
  ( Command (Command, commandName, commandHandler),
    withParsedArgs,
    makeInvalidParamsError,
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, Result (Error, Success), Value, fromJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.LSP.ServerMonads (ServerM)

-- | Command name and handler. When a 'LSP.WorkspaceExecuteCommand' request is
-- received with the command name matching the one listed in a 'Command', the
-- corresponding handler is executed.
data Command = Command
  { commandName :: Text,
    commandHandler :: LSP.Handler ServerM 'LSP.WorkspaceExecuteCommand
  }

-- | @withParsedArgs request respond handleUsingArgs@ parses the arguments list
-- of a 'LSP.WorkspaceExecuteCommand' request according to the following rules
-- and passes the parsed arguments to @handleUsingArgs@.
--
-- Parsing rules:
-- - The request contains exactly one JSON argument value.
-- - The single JSON argument can be parsed into the type that @handleUsingArgs@
--   expects to be passed.
--
-- When a request does not meet these requirements, a 'LSP.ResponseError' is
-- sent to the client and @handleUsingArgs@ is not run.
--
-- == Usage
-- This function is inteneded to be wrapped around the top-level of a command
-- handler:
--
-- @
-- data Args = Args { message :: String } deriving (Generic, FromJSON)
--
-- handle request response = withParsedArgs request response $ \args -> do
--   logM $ "received message " <> message args
--   -- ...
-- @
withParsedArgs ::
  (FromJSON args, LSP.MonadLsp c m) =>
  -- | LSP 'request'.
  LSP.RequestMessage 'LSP.WorkspaceExecuteCommand ->
  -- | LSP 'respond'.
  (Either LSP.ResponseError Value -> m ()) ->
  -- | Handler that need arguments.
  (args -> m ()) ->
  m ()
withParsedArgs request respond handleCmdUsingArgs = case request ^. LSP.params . LSP.arguments of
  Just (LSP.List [jsonArgument]) -> case fromJSON jsonArgument of
    Error err -> respond $ Left $ makeInvalidParamsError $ Text.pack err
    Success parsedArgs -> handleCmdUsingArgs parsedArgs
  _ -> respond $ Left $ makeInvalidParamsError "Expected exactly one argument"

makeInvalidParamsError :: Text -> LSP.ResponseError
makeInvalidParamsError msg =
  LSP.ResponseError
    { _code = LSP.InvalidParams,
      _message = msg,
      _xdata = Nothing
    }
