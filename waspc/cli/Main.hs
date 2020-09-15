module Main where

import System.Environment

import Command (runCommand)
import Command.CreateNewProject (createNewProject)
import Command.Start (start)
import Command.Clean (clean)
import Command.Db.Migrate (migrate)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["new", projectName] -> runCommand $ createNewProject projectName
        ["start"] -> runCommand start
        ["clean"] -> runCommand clean
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
    ["migrate", migrationName] -> runCommand $ migrate migrationName
    _ -> printDbUsage

printDbUsage :: IO ()
printDbUsage = putStrLn "This is how to use db commands:"
