module Wasp.Util.TaskStatus
  ( runTaskWithStatusReport,
  )
where

import qualified Wasp.Message as Msg
import qualified Wasp.Util.Interactive as Interactive

runTaskWithStatusReport :: Msg.SendMessage -> String -> String -> String -> IO (Either String a) -> IO (Either String a)
runTaskWithStatusReport sendMessage spinnerMessage successMessage failureMessage task = do
  interactiveTerminal <- Interactive.getInteractiveTerminal
  taskResult <- case interactiveTerminal of
    Just terminal -> Interactive.runTaskWithSpinner terminal spinnerMessage task
    Nothing -> sendMessage (Msg.Start spinnerMessage) >> task

  sendMessage $ case taskResult of
    Right _ -> Msg.Success successMessage
    Left errorMessage -> Msg.Failure failureMessage errorMessage
  return taskResult
