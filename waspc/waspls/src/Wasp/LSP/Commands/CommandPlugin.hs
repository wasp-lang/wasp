{-# LANGUAGE DataKinds #-}

module Wasp.LSP.Commands.CommandPlugin
  ( CommandPlugin (CommandPlugin, commandName, commandHandler),
    withParsedArgs,
    invalidParams,
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
-- received with the command name matching the one listed in a 'CommandPlugin',
-- the corresponding handler is executed.
data CommandPlugin = CommandPlugin
  { commandName :: Text,
    commandHandler :: LSP.Handler ServerM 'LSP.WorkspaceExecuteCommand
  }

-- | @withParsedArgs request respond run@ parses args from a 'LSP.WorkspaceExecuteCommand'
-- request and passes them to @run@. If an error occurs during parsing, responds
-- with an error and does not execute @run@.
withParsedArgs ::
  (FromJSON args, LSP.MonadLsp c m) =>
  -- | LSP 'request'.
  LSP.RequestMessage 'LSP.WorkspaceExecuteCommand ->
  -- | LSP 'respond'.
  (Either LSP.ResponseError Value -> m ()) ->
  -- | Handler that need arguments.
  (args -> m ()) ->
  m ()
withParsedArgs request respond run = case request ^. LSP.params . LSP.arguments of
  Just (LSP.List [argument]) -> case fromJSON argument of
    Error err -> respond $ Left $ invalidParams $ Text.pack err
    Success args -> run args
  _ -> respond $ Left $ invalidParams "Expected exactly one argument"

invalidParams :: Text -> LSP.ResponseError
invalidParams msg =
  LSP.ResponseError
    { _code = LSP.InvalidParams,
      _message = msg,
      _xdata = Nothing
    }
