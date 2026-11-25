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
import Wasp.Cli.Console.Reporter (Reporter (..), TaskEnd (..))
import qualified Wasp.Message as Msg
import Wasp.Problem (ToolOutput (..))
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
    runTask :: (MonadIO m, MonadMask m) => String -> (a -> TaskEnd) -> String -> m a -> m a
    runTask runningText describeTaskEnd failedText task =
      fst
        <$> generalBracket
          (liftIO $ announceTask runningText)
          (\_ taskExitCase -> liftIO $ concludeTask >> reportTaskEnd describeTaskEnd failedText taskExitCase)
          (const task)

    -- The interactive reporter shows a running task on the live line, which
    -- leaves nothing behind. The plain one writes a line that stays.
    (announceTask, concludeTask) = case outputStyle of
      Styled -> (Terminal.showLiveLine terminal, Terminal.hideLiveLine terminal)
      Plain -> (Terminal.writeLine terminal . renderTaskStart outputStyle, return ())

    reportTaskEnd :: (a -> TaskEnd) -> String -> ExitCase a -> IO ()
    reportTaskEnd describeTaskEnd failedText = \case
      ExitCaseSuccess taskResult -> case describeTaskEnd taskResult of
        TaskSucceeded successText -> Terminal.writeLine terminal $ renderMessage outputStyle $ Msg.Success successText
        TaskFailed resultFailedText -> writeFailedLine resultFailedText
      ExitCaseException exception
        -- The task didn't fail, it was stopped from the outside (e.g. Ctrl+C).
        | isJust (fromException exception :: Maybe SomeAsyncException) -> return ()
      _failed -> writeFailedLine failedText

    writeFailedLine = Terminal.writeLine terminal . renderTaskFailure outputStyle

renderMessage :: OutputStyle -> Msg.Message -> String
renderMessage outputStyle =
  renderInOutputStyle outputStyle . \case
    Msg.Info text -> text
    Msg.Success text -> TextStyle.styleSuccess "✔" ++ " " ++ text
    Msg.Failure headline details maybeToolOutput ->
      renderLabeledMessage errorLabel headline $ details `followedBy` maybe "" (renderToolOutput TextStyle.styleError) maybeToolOutput
    Msg.Warning headline details maybeToolOutput ->
      renderLabeledMessage warningLabel headline $ details `followedBy` maybe "" (renderToolOutput TextStyle.styleWarning) maybeToolOutput
  where
    errorLabel = renderLabel outputStyle TextStyle.styleError TextStyle.styleErrorLabel "ERROR"
    warningLabel = renderLabel outputStyle TextStyle.styleWarning TextStyle.styleWarningLabel "WARNING"

    followedBy "" text = text
    followedBy text "" = text
    followedBy text moreText = text ++ "\n" ++ moreText

renderTaskStart :: OutputStyle -> String -> String
renderTaskStart outputStyle runningText = renderInOutputStyle outputStyle $ "▶ " ++ runningText

renderTaskFailure :: OutputStyle -> String -> String
renderTaskFailure outputStyle failedText = renderInOutputStyle outputStyle $ TextStyle.styleError "✖" ++ " " ++ failedText

renderLabeledMessage :: String -> String -> String -> String
renderLabeledMessage label headline details =
  intercalate "\n" $ (label ++ " " ++ TextStyle.styleTitle headline) : [indent 2 trimmedDetails | not (null trimmedDetails)]
  where
    trimmedDetails = dropWhileEnd isSpace details

-- | Shows what the tool printed behind a gutter, which sets it apart from what
-- Wasp itself says.
renderToolOutput :: (String -> String) -> ToolOutput -> String
renderToolOutput styleGutter failedToolOutput =
  intercalate "\n" $
    ("Output of " ++ TextStyle.styleCode failedToolOutput.command ++ ":")
      : map renderOutputLine (lines failedToolOutput.output)
      ++ [styleGutter "└─" ++ " exited with code " ++ show failedToolOutput.exitCode]
  where
    renderOutputLine "" = styleGutter "│"
    renderOutputLine outputLine = styleGutter "│" ++ " " ++ outputLine

-- | The label stands on a coloured background. Where that can't be shown, a
-- colon sets it apart from the text after it.
renderLabel :: OutputStyle -> (String -> String) -> (String -> String) -> String -> String
renderLabel Styled styleSymbol styleLabel labelName = styleSymbol labelSymbol ++ " " ++ styleLabel (" " ++ labelName ++ " ")
renderLabel Plain _ _ labelName = labelSymbol ++ " " ++ labelName ++ ":"

labelSymbol :: String
labelSymbol = "!"
