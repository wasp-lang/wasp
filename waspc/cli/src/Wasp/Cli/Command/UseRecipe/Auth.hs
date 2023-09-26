{-# LANGUAGE InstanceSigs #-}

module Wasp.Cli.Command.UseRecipe.Auth where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (fromList)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.UseRecipe.Auth.Email (useEmail)
import Wasp.Cli.Command.UseRecipe.Auth.Local (useLocal)
import Wasp.Cli.Command.UseRecipe.Auth.Social (useGithub, useGoogle)
import qualified Wasp.Cli.Interactive as Interactive
import qualified Wasp.Message as Msg

data AuthMethod = Email | UsernameAndPassword | Google | Github

instance Show AuthMethod where
  show Email = "Email"
  show UsernameAndPassword = "Username and password"
  show Google = "Google"
  show Github = "Github"

instance Interactive.Option AuthMethod where
  showOption :: AuthMethod -> String
  showOption = show
  showOptionDescription :: AuthMethod -> Maybe String
  showOptionDescription _ = Nothing

useAuth :: Command ()
useAuth = do
  method <- liftIO selectMethod

  cliSendMessageC $ Msg.Start $ "Installing " <> show method <> " authentication..."
  useMethod method
  where
    useMethod Email = useEmail
    useMethod UsernameAndPassword = useLocal
    useMethod Google = useGoogle
    useMethod Github = useGithub

    methods =
      [ Email,
        UsernameAndPassword,
        Google,
        Github
      ]

    selectMethod =
      Interactive.askToChoose
        "Which authentication method do you want to use?"
        (fromList methods)
        Interactive.ChooserConfig {Interactive.hasDefaultOption = False}
