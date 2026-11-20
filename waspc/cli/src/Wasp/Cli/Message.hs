module Wasp.Cli.Message (cliSendMessage) where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.Extra (replace)
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
cliSendMessage (Msg.Failure entries) =
  waspScreams =<< labeledMessage Term.Red "!" "ERROR" entries
cliSendMessage (Msg.Warning entries) =
  waspWarns =<< labeledMessage Term.Yellow "!" "WARNING" entries

-- | Every entry starts with the label and everything else it says hangs
-- indented under that line. A one-line part continues the line instead, as
-- the next sentence.
labeledMessage :: Term.Style -> String -> String -> [Msg.Entry] -> IO String
labeledMessage color symbol label entries =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> render bold $ Term.applyStyles [Term.Bold, color] symbol ++ " " ++ Term.applyStyles [Term.Bold, Term.Reverse, color] (" " ++ label ++ " ")
    Nothing -> render id $ symbol ++ " " ++ label ++ ":"
  where
    render emphasized labelText = intercalate "\n" $ concatMap renderEntry entries
      where
        renderEntry (Msg.Entry text parts) = case foldl addPart (lines text) parts of
          [] -> [labelText]
          (firstLine : rest) -> (labelText ++ " " ++ emphasized firstLine) : map (indent 2) rest

        addPart entryLines (Msg.Text text) = case (entryLines, lines text) of
          ([firstLine], [oneLine]) -> [firstLine `continuedBy` oneLine]
          (_, textLines) -> entryLines ++ textLines

        continuedBy "" more = more
        continuedBy line more
          | last line `elem` (".!?:" :: String) = line ++ " " ++ more
          | otherwise = line ++ ". " ++ more

    -- Styled words in the line end with a reset, so bold is re-applied after each one.
    bold line = Term.applyStyles [Term.Bold] $ replace "\ESC[0m" "\ESC[0m\ESC[1m" line

styleIfInteractive :: [Term.Style] -> String -> IO String
styleIfInteractive styles text =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> Term.applyStyles styles text
    Nothing -> text
