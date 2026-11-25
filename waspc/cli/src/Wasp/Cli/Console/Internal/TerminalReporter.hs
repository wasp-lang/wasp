-- | The reporter that writes to the terminal. Only "Wasp.Cli.Console" uses it,
-- when it builds the console.
module Wasp.Cli.Console.Internal.TerminalReporter
  ( makeTerminalReporter,
    renderMessage,
    renderTaskStart,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Wasp.Cli.Console.Internal.OutputStyle (OutputStyle (..), renderInOutputStyle)
import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg
import Wasp.Util (indent)
import qualified Wasp.Util.TextStyle as TextStyle

-- | The interactive reporter ('Styled') is for a terminal that a user is
-- looking at. The plain one is for output that ends up in a file, a pipe, or a
-- terminal that can't show styles.
makeTerminalReporter :: OutputStyle -> Reporter
makeTerminalReporter outputStyle =
  Reporter
    { send = Terminal.writeLine . renderMessage outputStyle,
      runTaskWithStatusReport = runTaskByWritingStartAndSuccessLines
    }
  where
    runTaskByWritingStartAndSuccessLines :: (MonadIO m) => String -> String -> m a -> m a
    runTaskByWritingStartAndSuccessLines runningText successText task = do
      liftIO $ Terminal.writeLine $ renderTaskStart outputStyle runningText
      result <- task
      liftIO $ Terminal.writeLine $ renderMessage outputStyle $ Msg.Success successText
      return result

renderMessage :: OutputStyle -> Msg.Message -> String
renderMessage outputStyle =
  renderInOutputStyle outputStyle . \case
    Msg.Info text -> text
    Msg.Success text -> TextStyle.styleSuccess "✔" ++ " " ++ text
    Msg.Failure headline details -> renderLabeledMessage errorLabel headline details
    Msg.Warning headline details -> renderLabeledMessage warningLabel headline details
  where
    errorLabel = renderLabel outputStyle TextStyle.styleError TextStyle.styleErrorLabel "ERROR"
    warningLabel = renderLabel outputStyle TextStyle.styleWarning TextStyle.styleWarningLabel "WARNING"

renderTaskStart :: OutputStyle -> String -> String
renderTaskStart outputStyle runningText = renderInOutputStyle outputStyle $ "▶ " ++ runningText

renderLabeledMessage :: String -> String -> String -> String
renderLabeledMessage label headline details =
  intercalate "\n" $ (label ++ " " ++ TextStyle.styleTitle headline) : [indent 2 trimmedDetails | not (null trimmedDetails)]
  where
    trimmedDetails = dropWhileEnd isSpace details

-- | The label stands on a coloured background. Where that can't be shown, a
-- colon sets it apart from the text after it.
renderLabel :: OutputStyle -> (String -> String) -> (String -> String) -> String -> String
renderLabel Styled styleSymbol styleLabel labelName = styleSymbol labelSymbol ++ " " ++ styleLabel (" " ++ labelName ++ " ")
renderLabel Plain _ _ labelName = labelSymbol ++ " " ++ labelName ++ ":"

labelSymbol :: String
labelSymbol = "!"
