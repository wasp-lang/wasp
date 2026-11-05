module Wasp.Util.TaskStatus
  ( runTaskWithStatusReport,
  )
where

import qualified Wasp.Util.Interactive as Interactive
import qualified Wasp.Util.Terminal as Term

-- | Runs a task that prints nothing itself and reports on it: a status line
-- with the loading message while it runs, and one with the success or failure
-- message once it is done.
--
-- On an interactive terminal the loading line spins and is replaced by the
-- final line. Otherwise the two lines are printed one after the other, so logs
-- stay readable.
runTaskWithStatusReport :: String -> String -> String -> IO (Either e a) -> IO (Either e a)
runTaskWithStatusReport loadingMessage successMessage failureMessage task = do
  result <-
    Interactive.getInteractiveTerminal >>= \case
      Just terminal -> Interactive.withLongRunningTask terminal loadingMessage task
      Nothing -> do
        putStrLn $ "▶ " ++ loadingMessage
        task
  putStrLn $ case result of
    Right _ -> Term.applyStyles [Term.Green] "✔" ++ " " ++ successMessage
    Left _ -> Term.applyStyles [Term.Red] "▲ error" ++ " " ++ failureMessage
  return result
