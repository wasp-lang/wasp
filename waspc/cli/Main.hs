module Main where

import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (void)
import           Data.Version             (showVersion)
import           Paths_waspc              (version)
import           System.Environment

import           Command                  (runCommand)
import           Command.Build            (build)
import qualified Command.Call
import           Command.Clean            (clean)
import           Command.Compile          (compile)
import           Command.CreateNewProject (createNewProject)
import           Command.Db               (runDbCommand, studio)
import           Command.Db.Migrate       (migrateSave, migrateUp)
import           Command.Start            (start)
import qualified Command.Telemetry        as Telemetry


main :: IO ()
main = do
    args <- getArgs
    let commandCall = case args of
            ["new", projectName] -> Command.Call.New projectName
            ["start"]            -> Command.Call.Start
            ["clean"]            -> Command.Call.Clean
            ["compile"]          -> Command.Call.Compile
            ("db":dbArgs)        -> Command.Call.Db dbArgs
            ["version"]          -> Command.Call.Version
            ["build"]            -> Command.Call.Build
            _                    -> Command.Call.Unknown args

    telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall

    case commandCall of
        Command.Call.New projectName -> runCommand $ createNewProject projectName
        Command.Call.Start -> runCommand start
        Command.Call.Clean -> runCommand clean
        Command.Call.Compile -> runCommand compile
        Command.Call.Db dbArgs -> dbCli dbArgs
        Command.Call.Version -> printVersion
        Command.Call.Build -> runCommand build
        Command.Call.Unknown _ -> printUsage

    -- If sending of telemetry data is still not done 1 second since commmand finished, abort it.
    -- We also make sure here to catch all errors that might get thrown and silence them.
    void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
      threadDelaySeconds = let microsecondsInASecond = 1000000
                           in threadDelay . (* microsecondsInASecond)

printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "Usage:"
    , "  wasp <command> [command-args]"
    , ""
    , "Commands:"
    , "  new <project-name>"
    , "  start"
    , "  clean"
    , "  db <commmand> [command-args]"
    , "  build"
    , "  version"
    , ""
    , "Examples:"
    , "  wasp new MyApp"
    , "  wasp start"
    , "  wasp db migrate-save \"init\""
    , ""
    , "Documentation is available at https://wasp-lang.dev/docs ."
    ]

printVersion :: IO ()
printVersion = putStrLn $ showVersion version

-- TODO(matija): maybe extract to a separate module, e.g. DbCli.hs?
dbCli :: [String] -> IO ()
dbCli args = case args of
    ["migrate-save", migrationName] -> runDbCommand $ migrateSave migrationName
    ["migrate-up"] -> runDbCommand migrateUp
    ["studio"] -> runDbCommand studio
    _ -> printDbUsage

printDbUsage :: IO ()
printDbUsage = putStrLn $ unlines
    [ "Usage:"
    , "  wasp db <command> [command-args]"
    , ""
    , "Commands:"
    , "  migrate-save <migration-name>"
    , "  migrate-up"
    , "  studio"
    , ""
    , "Examples:"
    , "  wasp db migrate-save \"Added description field.\""
    , "  wasp db migrate-up"
    ]
