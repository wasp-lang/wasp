module Main where

import System.Environment

import Command (runCommand)
import Command.CreateNewProject (createNewProject)
import Command.Start (start)
import Command.Clean (clean)
import Command.Compile (compile)
import Command.Db.Migrate (migrateSave, migrateUp)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["new", projectName] -> runCommand $ createNewProject projectName
        ["start"] -> runCommand start
        ["clean"] -> runCommand clean
        ["compile"] -> runCommand compile
        ("db":dbArgs) -> dbCli dbArgs
        _ -> printUsage

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
    , ""
    , "Examples:"
    , "  wasp create MyApp"
    , "  wasp start"
    ]

-- TODO(matija): maybe extract to a separate module, e.g. DbCli.hs?
dbCli :: [String] -> IO ()
dbCli args = case args of
    ["migrate-save", migrationName] -> runCommand $ migrateSave migrationName
    ["migrate-up"] -> runCommand $ migrateUp
    _ -> printDbUsage

printDbUsage :: IO ()
printDbUsage = putStrLn $ unlines
    [ "Usage:"
    , "  wasp db <command> [command-args]"
    , ""
    , "Commands:"
    , "  migrate-save <migration-name>"
    , "  migrate-up"
    , ""
    , "Examples:"
    , "  wasp db migrate-save \"Added description field.\""
    , "  wasp db migrate-up"
    ]
