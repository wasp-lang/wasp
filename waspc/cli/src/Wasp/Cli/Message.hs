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
cliSendMessage (Msg.Failure title msg) =
  waspScreams =<< labeledMessage Term.Red "✖" "ERROR" title msg
cliSendMessage (Msg.Warning title msg) =
  waspWarns =<< labeledMessage Term.Yellow "!" "WARNING" title msg

labeledMessage :: Term.Style -> String -> String -> String -> String -> IO String
labeledMessage color symbol label title details =
  Interactive.getInteractiveTerminal <&> \case
    Just _ ->
      withDetails
        ( Term.applyStyles [Term.Bold, color] symbol
            ++ " "
            ++ Term.applyStyles [Term.Bold, Term.Reverse, color] (" " ++ label ++ " ")
            ++ " "
            ++ Term.applyStyles [color] title
        )
        details
    Nothing -> withDetails (symbol ++ " " ++ label ++ ": " ++ title) details

withDetails :: String -> String -> String
withDetails title "" = title
withDetails title details = title ++ "\n" ++ indent 2 details

styleIfInteractive :: [Term.Style] -> String -> IO String
styleIfInteractive styles text =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> Term.applyStyles styles text
    Nothing -> text
