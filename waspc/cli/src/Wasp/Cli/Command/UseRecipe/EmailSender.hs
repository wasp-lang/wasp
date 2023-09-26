module Wasp.Cli.Command.UseRecipe.EmailSender where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (fromList)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.UseRecipe.Common (appendToServerEnv)
import qualified Wasp.Cli.Interactive as Interactive
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

data EmailProvider = SMTP | SendGrid | Mailgun

instance Show EmailProvider where
  show SMTP = "SMTP"
  show SendGrid = "SendGrid"
  show Mailgun = "Mailgun"

instance Interactive.Option EmailProvider where
  showOption = show

  showOptionDescription _ = Nothing

useEmailSender :: Command ()
useEmailSender = do
  provider <- liftIO selectProvider

  cliSendMessageC $ Msg.Start $ "Setting up " <> show provider <> " email sender..."

  useProvider provider
  where
    providers =
      [ SMTP,
        SendGrid,
        Mailgun
      ]
    selectProvider =
      Interactive.askToChoose
        "Which email sender do you want to use?"
        (fromList providers)
        Interactive.ChooserConfig {Interactive.hasDefaultOption = False}

    useProvider :: EmailProvider -> Command ()
    useProvider SMTP = useEmailProvider "SMTP" ["SMTP_HOST", "SMTP_PORT", "SMTP_USERNAME", "SMTP_PASSWORD"]
    useProvider SendGrid = useEmailProvider "SendGrid" ["SENDGRID_API_KEY"]
    useProvider Mailgun = useEmailProvider "Mailgun" ["MAILGUN_API_KEY", "MAILGUN_DOMAIN"]

useEmailProvider :: String -> [String] -> Command ()
useEmailProvider providerName providerEnvVariableNames = do
  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following email sender block to the app block in your main.wasp file:\n"
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "emailSender: {",
          "  provider: " <> providerName,
          "}"
        ]

  appendToServerEnv $ unlines $ map (<> "=\"\"") providerEnvVariableNames
  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] $ "Fill the values for " <> joinForSentence providerEnvVariableNames <> " in .env.server file."

joinForSentence :: [String] -> String
joinForSentence [] = ""
joinForSentence [x] = x
joinForSentence [x, y] = x <> " and " <> y
joinForSentence (x : xs) = x <> ", " <> joinForSentence xs
