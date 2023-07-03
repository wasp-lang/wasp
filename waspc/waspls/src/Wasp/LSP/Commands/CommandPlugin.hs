{-# LANGUAGE DataKinds #-}

module Wasp.LSP.Commands.CommandPlugin
  ( CommandPlugin (CommandPlugin, commandName, commandHandler),
  )
where

import Data.Text (Text)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import Wasp.LSP.ServerM (ServerM)

data CommandPlugin = CommandPlugin
  { commandName :: Text,
    commandHandler :: LSP.Handler ServerM 'LSP.WorkspaceExecuteCommand
  }
