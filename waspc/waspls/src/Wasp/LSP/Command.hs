module Wasp.LSP.Command
  ( -- * waspls Commands

    -- Executes commands that have been defined as command plugins.
    --
    -- To define a new command, create a "Wasp.LSP.Commands.CommandPlugin" for
    -- it and add the plugin to 'plugins' in this module.
    --
    -- When defining a new command, it is recommended to, in addition to the
    -- 'CommandPlugin', define an @Args@ type that the command expects to be
    -- passed to it and a @command@ function that takes an @Args@ value and
    -- returns an 'LSP.Command'. This makes it simpler to call the command
    -- correctly.
    availableCommands,
    handler,
  )
where

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Text.Printf (printf)
import Wasp.LSP.Commands.CommandPlugin (CommandPlugin (commandHandler, commandName))
import qualified Wasp.LSP.Commands.ScaffoldTsSymbol as ScaffoldTsSymbol
import Wasp.LSP.ServerMonads (ServerM)

plugins :: M.HashMap Text CommandPlugin
plugins =
  M.fromList $
    map
      (commandName &&& id)
      [ ScaffoldTsSymbol.plugin
      ]

-- | List of the names of commands that 'handler' can execute.
availableCommands :: [Text]
availableCommands = M.keys plugins

-- | Find the relevant 'CommandPlugin' in 'plugins' for the request, or respond
-- with an error if there is no handler listed for it.
handler :: LSP.Handlers ServerM
handler = LSP.requestHandler LSP.SWorkspaceExecuteCommand $ \request respond ->
  let command = request ^. LSP.params . LSP.command
   in case plugins M.!? command of
        Nothing -> do
          respond $
            Left $
              LSP.ResponseError
                { _code = LSP.MethodNotFound,
                  _message = Text.pack $ printf "No handler for command '%s'" command,
                  _xdata = Nothing
                }
        Just plugin -> commandHandler plugin request respond
