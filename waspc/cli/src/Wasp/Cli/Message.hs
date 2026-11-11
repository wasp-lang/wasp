module Wasp.Cli.Message (cliSendMessage) where

import Data.Functor ((<&>))
import Wasp.Cli.Common (waspSays, waspScreams, waspWarns)
import qualified Wasp.Message as Msg
import Wasp.Util (indent)
import qualified Wasp.Util.Interactive as Interactive
import qualified Wasp.Util.Terminal as Term

-- | Send a message using the CLI
cliSendMessage :: Msg.SendMessage
cliSendMessage (Msg.Info msg) =
  waspSays msg
cliSendMessage (Msg.Start msg) =
  waspSays $ "▶ " ++ msg
cliSendMessage (Msg.Success msg) = do
  check <- styleIfInteractive [Term.Green] "✔"
  waspSays $ check ++ " " ++ msg
cliSendMessage (Msg.Failure title msg) = do
  label <- styleIfInteractive [Term.Red, Term.Bold] "✖ Error:"
  waspScreams $ label ++ " " ++ withDetails title msg
cliSendMessage (Msg.Warning title msg) = do
  label <- styleIfInteractive [Term.Yellow, Term.Bold] "! Warning:"
  waspWarns $ label ++ " " ++ withDetails title msg

withDetails :: String -> String -> String
withDetails title "" = title
withDetails title details = title ++ "\n" ++ indent 2 details

styleIfInteractive :: [Term.Style] -> String -> IO String
styleIfInteractive styles text =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> Term.applyStyles styles text
    Nothing -> text
