module Wasp.Cli.Message (cliSendMessage) where

import Data.Functor ((<&>))
import Data.List (intercalate)
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
cliSendMessage (Msg.Failure title details) =
  waspScreams =<< labeledMessage Term.Red "✖" "ERROR" title details
cliSendMessage (Msg.Warning title details) =
  waspWarns =<< labeledMessage Term.Yellow "!" "WARNING" title details

labeledMessage :: Term.Style -> String -> String -> String -> [Msg.Part] -> IO String
labeledMessage color symbol label title details =
  Interactive.getInteractiveTerminal <&> \case
    Just _ ->
      withDetails
        (Just color)
        ( Term.applyStyles [Term.Bold, color] symbol
            ++ " "
            ++ Term.applyStyles [Term.Bold, Term.Reverse, color] (" " ++ label ++ " ")
            ++ " "
            ++ title
        )
        details
    Nothing -> withDetails Nothing (symbol ++ " " ++ label ++ ": " ++ title) details

withDetails :: Maybe Term.Style -> String -> [Msg.Part] -> String
withDetails color title parts = intercalate "\n" $ title : concatMap renderPart parts
  where
    renderPart = \case
      Msg.Text text -> [indent 2 $ marker ++ " " ++ text]
      Msg.ToolOutput command output ->
        [ indent 2 $ marker ++ " Output of " ++ Term.styleCode command ++ ":",
          indent 2 $ intercalate "\n" $ map gutterLine $ lines output
        ]

    gutterLine "" = gutter
    gutterLine line = gutter ++ " " ++ line
    marker = colored ">"
    gutter = colored "│"
    colored = maybe id (\c -> Term.applyStyles [c]) color

styleIfInteractive :: [Term.Style] -> String -> IO String
styleIfInteractive styles text =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> Term.applyStyles styles text
    Nothing -> text
