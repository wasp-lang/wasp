-- | The reporter that writes to the terminal. Only "Wasp.Cli.Console" uses it,
-- when it builds the console.
module Wasp.Cli.Console.Internal.TerminalReporter
  ( makeTerminalReporter,
    renderMessage,
    renderTaskStart,
    renderTaskFailure,
  )
where

import Control.Exception (SomeAsyncException, fromException)
import Control.Monad.Catch (ExitCase (..), MonadMask, generalBracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (isJust)
import Wasp.Cli.Console.Internal.OutputStyle (OutputStyle (..), renderInOutputStyle)
import Wasp.Cli.Console.Internal.Terminal (Terminal)
import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg
import Wasp.Util (indent)
import qualified Wasp.Util.TextStyle as TextStyle

-- | The interactive reporter ('Styled') is for a terminal that a user is
-- looking at. The plain one is for output that ends up in a file, a pipe, or a
-- terminal that can't show styles.
makeTerminalReporter :: Terminal -> OutputStyle -> Reporter
makeTerminalReporter terminal outputStyle =
  Reporter
    { send = Terminal.writeLine terminal . renderMessage outputStyle,
      runTaskWithStatusReport = runTask
    }
  where
    runTask :: (MonadIO m, MonadMask m) => String -> String -> String -> m a -> m a
    runTask runningText successText failedText task =
      fst
        <$> generalBracket
          (liftIO $ announceTask runningText)
          (\_ taskExitCase -> liftIO $ concludeTask >> reportTaskEnd successText failedText taskExitCase)
          (const task)

    -- The interactive reporter shows a running task on the live line, which
    -- leaves nothing behind. The plain one writes a line that stays.
    (announceTask, concludeTask) = case outputStyle of
      Styled -> (Terminal.showLiveLine terminal, Terminal.hideLiveLine terminal)
      Plain -> (Terminal.writeLine terminal . renderTaskStart outputStyle, return ())

    reportTaskEnd :: String -> String -> ExitCase a -> IO ()
    reportTaskEnd successText failedText = \case
      ExitCaseSuccess _ -> Terminal.writeLine terminal $ renderMessage outputStyle $ Msg.Success successText
      ExitCaseException exception
        -- The task didn't fail, it was stopped from the outside (e.g. Ctrl+C).
        | isJust (fromException exception :: Maybe SomeAsyncException) -> return ()
      _failed -> Terminal.writeLine terminal $ renderTaskFailure outputStyle failedText

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

renderTaskFailure :: OutputStyle -> String -> String
renderTaskFailure outputStyle failedText = renderInOutputStyle outputStyle $ TextStyle.styleError "✖" ++ " " ++ failedText

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
