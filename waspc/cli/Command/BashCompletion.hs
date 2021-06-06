module Command.BashCompletion
  ( bashCompletion,
  )
where

import Command (Command)
import Control.Monad.IO.Class (liftIO)

bashCompletion :: [String] -> Command ()
bashCompletion args = case args of
  [] -> listCommands
  ["db"] -> listCommandsDb
  _ -> liftIO . putStrLn $ unlines []

-- return available options for wasp <command>
listCommands :: Command ()
listCommands = liftIO . putStrLn $ unlines ["new", "version", "start", "db", "clean", "build", "telemetry", "deps"]

-- return available options for wasp db <command>
listCommandsDb :: Command ()
listCommandsDb = liftIO . putStrLn $ unlines ["migrate-dev", "studio"]
