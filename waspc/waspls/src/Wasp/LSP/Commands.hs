module Wasp.LSP.Commands
  ( -- * waspls Commands

    -- Executes commands that have been defined as command plugins.
    --
    -- To define a new command, create a "Wasp.LSP.Commands.CommandPlugin" for
    -- it and add the plugin to 'plugins' in this module.
    --
    -- When defining a new command, it is recommended to, in addition to the
    -- 'Command', define an @Args@ type that the command expects to be
    -- passed to it and a @lspCommand@ function that takes an @Args@ value and
    -- returns an 'LSP.Command'. Following this pattern will ensure a simple
    -- and consistent interface to interacting with each command.
    availableCommands,
    handleExecuteCommand,
  )
where

import Control.Lens ((^.))
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Text.Printf (printf)
import Wasp.LSP.Commands.Command (Command (commandHandler, commandName))
import qualified Wasp.LSP.Commands.ScaffoldTsSymbol as ScaffoldTsSymbol
import Wasp.LSP.ServerMonads (ServerM)

commands :: M.HashMap Text Command
commands =
  M.fromList $
    map
      (\plugin -> (commandName plugin, plugin))
      [ ScaffoldTsSymbol.command
      ]

-- | List of the names of commands that 'handler' can execute.
availableCommands :: [Text]
availableCommands = M.keys commands

-- | Find the relevant 'CommandPlugin' in 'plugins' for the request, or respond
-- with an error if there is no handler listed for it.
handleExecuteCommand :: LSP.Handlers ServerM
handleExecuteCommand = LSP.requestHandler LSP.SWorkspaceExecuteCommand $ \request respond ->
  let command = request ^. LSP.params . LSP.command
   in case commands M.!? command of
        Nothing -> do
          respond $
            Left $
              LSP.ResponseError
                { _code = LSP.MethodNotFound,
                  _message = Text.pack $ printf "No handler for command '%s'" command,
                  _xdata = Nothing
                }
        Just plugin -> commandHandler plugin request respond
