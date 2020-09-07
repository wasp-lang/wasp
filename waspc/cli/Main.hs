module Main where

import System.Environment

import Command (runCommand)
import Command.CreateNewProject (createNewProject)
import Command.Start (start)
import Command.Clean (clean)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["new", projectName] -> runCommand $ createNewProject projectName
        ["start"] -> runCommand start
        ["clean"] -> runCommand clean
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
    , ""
    , "Examples:"
    , "  wasp create MyApp"
    , "  wasp start"
    ]



