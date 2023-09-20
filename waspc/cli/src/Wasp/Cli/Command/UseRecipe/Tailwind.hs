module Wasp.Cli.Command.UseRecipe.Tailwind where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

useTailwind :: Command ()
useTailwind = do
  cliSendMessageC $ Msg.Start "Installing Tailwind..."

  -- TODO: copy the config files from the Cli/recipes/tailwind folder to the project dir.

  cliSendMessageC $ Msg.Success "Installed tailwind!"
