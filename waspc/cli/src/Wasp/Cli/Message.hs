module Wasp.Cli.Message (cliSendMessage) where

import Wasp.Cli.Common (waspSays, waspScreams, waspWarns)
import Wasp.Cli.Terminal
  ( asWaspFailureMessage,
    asWaspWarningMessage,
  )
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Interactive as Interactive
import qualified Wasp.Util.Terminal as Term

-- | Send a message using the CLI
cliSendMessage :: Msg.SendMessage
cliSendMessage (Msg.Info msg) =
  waspSays msg
cliSendMessage (Msg.Start msg) =
  waspSays $ "▶ " ++ msg
cliSendMessage (Msg.Success msg) = do
  interactiveTerminal <- Interactive.getInteractiveTerminal
  waspSays $ maybe id (const $ Term.applyStyles [Term.Green]) interactiveTerminal "✔" ++ " " ++ msg
cliSendMessage (Msg.Failure title msg) =
  waspScreams $ asWaspFailureMessage (title ++ ":") ++ msg
cliSendMessage (Msg.Warning title msg) =
  waspWarns $ asWaspWarningMessage (title ++ ":") ++ msg
