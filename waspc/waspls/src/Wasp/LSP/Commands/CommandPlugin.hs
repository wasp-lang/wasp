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

data CommandPlugin = CommandPlugin
  { commandName :: Text,
    commandHandler :: LSP.Handler ServerM 'LSP.WorkspaceExecuteCommand
  }

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
