module Command.BashCompletion
  ( bashCompletion,
  )
where

import qualified Cli.Common as Common
import Command (Command, CommandError (..))
import qualified Command.Common
import Control.Monad.IO.Class (liftIO)


bashCompletion :: [String] -> Command ()
bashCompletion args = case args of
  [] -> listCommands
  ["db"] -> listCommandsDb
  _ -> printNoSuggestion

-- return available options for wasp <command>
listCommands :: Command ()
listCommands = liftIO . putStrLn $ unlines ["new", "version", "start", "db", "clean", "build", "telemetry", "deps"]

-- return available options for wasp db <command>
listCommandsDb :: Command ()
listCommandsDb = liftIO . putStrLn $ unlines ["migrate-dev",  "studio"]

-- return an empty list if the command is unknown
printNoSuggestion :: Command ()
printNoSuggestion = liftIO . putStrLn $ unlines []
