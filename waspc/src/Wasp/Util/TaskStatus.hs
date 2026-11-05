module Wasp.Util.TaskStatus
  ( runTaskWithStatusReport,
  )
where

import qualified Wasp.Util.Interactive as Interactive
import qualified Wasp.Util.Terminal as Term

runTaskWithStatusReport :: String -> String -> String -> IO (Either e a) -> IO (Either e a)
runTaskWithStatusReport spinnerMessage successMessage failureMessage task = do
  interactiveTerminal <- Interactive.getInteractiveTerminal
  taskResult <- case interactiveTerminal of
    Just terminal -> Interactive.runTaskWithSpinner terminal spinnerMessage task
    Nothing -> putStrLn ("▶ " ++ spinnerMessage) >> task

  putStrLn $ case taskResult of
    Right _ -> Term.applyStyles [Term.Green] "✔" ++ " " ++ successMessage
    Left _ -> Term.applyStyles [Term.Red] "▲ error" ++ " " ++ failureMessage
  return taskResult
