module Wasp.Util.TaskStatus
  ( runTaskWithStatusReport,
  )
where

import Data.List (intercalate)
import qualified Wasp.Util.Interactive as Interactive
import qualified Wasp.Util.Terminal as Term

-- | Runs the task, telling the user it is running and then how it ended. On an
-- interactive terminal the running task is a spinner line, otherwise a "▶"
-- line without colors. A failed task's error is shown under its line.
runTaskWithStatusReport :: String -> String -> String -> IO (Either String a) -> IO (Either String a)
runTaskWithStatusReport runningMessage successMessage failureMessage task = do
  interactiveTerminal <- Interactive.getInteractiveTerminal
  taskResult <- case interactiveTerminal of
    Just terminal -> Interactive.runTaskWithSpinner terminal runningMessage task
    Nothing -> putStrLn ("▶ " ++ runningMessage) >> task

  let style = case interactiveTerminal of
        Just _ -> Term.applyStyles
        Nothing -> const id
      errorLabel = case interactiveTerminal of
        Just _ -> style [Term.Bold, Term.Red] "✖" ++ " " ++ style [Term.Bold, Term.Reverse, Term.Red] " ERROR "
        Nothing -> "✖ ERROR:"
  putStrLn $ case taskResult of
    Right _ -> style [Term.Green] "✔" ++ " " ++ successMessage
    Left errorMessage ->
      intercalate "\n" $
        (errorLabel ++ " " ++ style [Term.Red] failureMessage)
          : map (("  " ++ style [Term.Red] "│" ++ " ") ++) (lines errorMessage)
  return taskResult
