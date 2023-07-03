module Wasp.LSP.Command
  ( availableCommands,
    handler,
  )
where

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.LSP.Commands.CommandPlugin (CommandPlugin (commandHandler, commandName))
import qualified Wasp.LSP.Commands.ScaffoldTsSymbol as ScaffoldTsSymbol
import Wasp.LSP.ServerM (ServerM)

plugins :: M.HashMap Text CommandPlugin
plugins =
  M.fromList $
    map
      (commandName &&& id)
      [ ScaffoldTsSymbol.plugin
      ]

availableCommands :: [Text]
availableCommands = M.keys plugins

handler :: LSP.Handlers ServerM
handler = LSP.requestHandler LSP.SWorkspaceExecuteCommand $ \request respond ->
  case plugins M.!? (request ^. LSP.params . LSP.command) of
    Nothing -> do
      LSP.sendNotification LSP.SWindowShowMessage $
        LSP.ShowMessageParams
          { _xtype = LSP.MtError,
            _message = "waspls can not run the command " <> (request ^. LSP.params . LSP.command)
          }
    Just plugin -> commandHandler plugin request respond
