module Wasp.Cli.Command.UseRecipe.Auth where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (fromList)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Interactive as Interactive
import qualified Wasp.Message as Msg

data AuthMethod = Email | UsernameAndPassword | Google | Github

instance Show AuthMethod where
  show Email = "Email"
  show UsernameAndPassword = "Username and password"
  show Google = "Google"
  show Github = "Github"

instance Interactive.Option AuthMethod where
  showOption = show
  showOptionDescription _ = Nothing

useAuth :: Command ()
useAuth = do
  method <- liftIO selectMethod

  cliSendMessageC $ Msg.Start "Installing authentication..."

  -- Create React pages for each method
  -- Edit the Wasp file (or prompt the user to do so)

  cliSendMessageC $ Msg.Success $ "Installed " <> show method <> " authentication!"
  where
    methods =
      [ Email,
        UsernameAndPassword,
        Google,
        Github
      ]

    selectMethod =
      Interactive.askToChoose
        "What authentication method do you want to use?"
        (fromList methods)
        Interactive.ChooserConfig {Interactive.hasDefaultOption = False}
