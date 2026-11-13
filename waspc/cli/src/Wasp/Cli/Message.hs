module Wasp.Cli.Message (cliSendMessage, runStepGroup) where

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

-- | Runs the steps of a bigger step as one. In a terminal they share one
-- status line that follows the running step, their success lines are not
-- printed, and anything else they report goes above the line. Elsewhere the
-- group announces itself and the steps print their lines as usual. The
-- caller reports how the group ended.
runStepGroup :: String -> (Msg.SendMessage -> IO a) -> IO a
runStepGroup runningMessage steps =
  Interactive.getInteractiveTerminal >>= \case
    Just terminal -> Interactive.withStatusLine terminal runningMessage $ \statusLine ->
      steps $ \case
        Msg.Success _ -> return ()
        message -> Interactive.printAboveStatusLine statusLine $ cliSendMessage message
    Nothing -> cliSendMessage (Msg.Start runningMessage) >> steps cliSendMessage

-- | Every entry starts with the label and everything else it says hangs
-- indented under that line. A short part continues the line instead: a
-- one-line text joins it as the next sentence, and a tool output joins it as
-- "Output of `cmd`:", followed by the output behind a gutter.
labeledMessage :: Term.Style -> String -> String -> [Msg.Entry] -> IO String
labeledMessage color symbol label entries =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> render (Just color) $ Term.applyStyles [Term.Bold, color] symbol ++ " " ++ Term.applyStyles [Term.Bold, Term.Reverse, color] (" " ++ label ++ " ")
    Nothing -> render Nothing $ symbol ++ " " ++ label ++ ":"
  where
    render gutterColor labelText = intercalate "\n" $ concatMap renderEntry entries
      where
        renderEntry (Msg.Entry text parts) = case foldl addPart (lines text) parts of
          [] -> [labelText]
          (firstLine : rest) -> (labelText ++ " " ++ emphasized firstLine) : map (indent 2) rest

        emphasized = maybe id (const bold) gutterColor
        -- Styled words in the line end with a reset, so bold is re-applied after each one.
        bold line = Term.applyStyles [Term.Bold] $ replace "\ESC[0m" "\ESC[0m\ESC[1m" line

        addPart entryLines = \case
          Msg.Text text -> case (entryLines, lines text) of
            ([firstLine], [oneLine]) -> [firstLine `continuedBy` oneLine]
            (_, textLines) -> entryLines ++ textLines
          Msg.ToolOutput command exitCode output ->
            let heading = "Output of " ++ Term.styleCode command ++ ":"
                block = map gutterLine (lines output) ++ [corner ++ " exited with code " ++ show exitCode]
             in case entryLines of
                  [firstLine] -> (firstLine `continuedBy` heading) : block
                  _ -> entryLines ++ heading : block

        continuedBy "" more = more
        continuedBy line more
          | last line `elem` (".!?:" :: String) = line ++ " " ++ more
          | otherwise = line ++ ". " ++ more

        gutterLine "" = gutter
        gutterLine line = gutter ++ " " ++ line
        gutter = colored "│"
        corner = colored "└─"
        colored = maybe id (\c -> Term.applyStyles [c]) gutterColor

styleIfInteractive :: [Term.Style] -> String -> IO String
styleIfInteractive styles text =
  Interactive.getInteractiveTerminal <&> \case
    Just _ -> Term.applyStyles styles text
    Nothing -> text
