module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Char (isSpace)
import Main.Utf8 (withUtf8)
import System.Environment (getArgs)
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.BashCompletion (bashCompletion, generateBashCompletionScript, printBashCompletionInstruction)
import Wasp.Cli.Command.Build (build)
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import Wasp.Cli.Command.Db (runDbCommand, studio)
import qualified Wasp.Cli.Command.Db.Migrate as Command.Db.Migrate
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Cli.Terminal (title)
import Wasp.Util (indent)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  args <- getArgs
  let commandCall = case args of
        ["new", projectName] -> Command.Call.New projectName
        ["start"] -> Command.Call.Start
        ["clean"] -> Command.Call.Clean
        ["compile"] -> Command.Call.Compile
        ("db" : dbArgs) -> Command.Call.Db dbArgs
        ["version"] -> Command.Call.Version
        ["build"] -> Command.Call.Build
        ["telemetry"] -> Command.Call.Telemetry
        ["deps"] -> Command.Call.Deps
        ["dockerfile"] -> Command.Call.Dockerfile
        ["info"] -> Command.Call.Info
        ["completion"] -> Command.Call.PrintBashCompletionInstruction
        ["completion:generate"] -> Command.Call.GenerateBashCompletionScript
        ["completion:list"] -> Command.Call.BashCompletionListCommands
        ("waspls" : _) -> Command.Call.WaspLS
        _ -> Command.Call.Unknown args

  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall

  case commandCall of
    Command.Call.New projectName -> runCommand $ createNewProject projectName
    Command.Call.Start -> runCommand start
    Command.Call.Clean -> runCommand clean
    Command.Call.Compile -> runCommand compile
    Command.Call.Db dbArgs -> dbCli dbArgs
    Command.Call.Version -> printVersion
    Command.Call.Build -> runCommand build
    Command.Call.Telemetry -> runCommand Telemetry.telemetry
    Command.Call.Deps -> runCommand deps
    Command.Call.Dockerfile -> runCommand printDockerfile
    Command.Call.Info -> runCommand info
    Command.Call.PrintBashCompletionInstruction -> runCommand printBashCompletionInstruction
    Command.Call.GenerateBashCompletionScript -> runCommand generateBashCompletionScript
    Command.Call.BashCompletionListCommands -> runCommand bashCompletion
    Command.Call.Unknown _ -> printUsage
    Command.Call.WaspLS -> runWaspLS

  -- If sending of telemetry data is still not done 1 second since commmand finished, abort it.
  -- We also make sure here to catch all errors that might get thrown and silence them.
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = putStrLn $ "\nInternal Wasp error (bug in compiler):\n" ++ indent 2 (show e)

printUsage :: IO ()
printUsage =
  putStrLn $
    unlines
      [ title "USAGE",
        "  wasp <command> [command-args]",
        "",
        title "COMMANDS",
        title "  GENERAL",
        cmd "    new <project-name>    Creates new Wasp project.",
        cmd "    version               Prints current version of CLI.",
        cmd "    waspls                Run Wasp Language Server. Add --help to get more info.",
        cmd "    completion            Prints help on bash completion.",
        title "  IN PROJECT",
        cmd "    start                 Runs Wasp app in development mode, watching for file changes.",
        cmd "    db <db-cmd> [args]    Executes a database command. Run 'wasp db' for more info.",
        cmd "    clean                 Deletes all generated code and other cached artifacts. Wasp equivalent of 'have you tried closing and opening it again?'.",
        cmd "    build                 Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        cmd "    telemetry             Prints telemetry status.",
        cmd "    deps                  Prints the dependencies that Wasp uses in your project.",
        cmd "    dockerfile            Prints the contents of the Wasp generated Dockerfile.",
        cmd "    info                  Prints basic information about current Wasp project.",
        "",
        title "EXAMPLES",
        "  wasp new MyApp",
        "  wasp start",
        "  wasp db migrate-dev",
        "",
        Term.applyStyles [Term.Green] "Docs:" ++ " https://wasp-lang.dev/docs",
        Term.applyStyles [Term.Magenta] "Discord (chat):" ++ " https://discord.gg/rzdnErX",
        Term.applyStyles [Term.Cyan] "Newsletter:" ++ " https://wasp-lang.dev/#signup"
      ]

printVersion :: IO ()
printVersion = do
  putStrLn $
    unlines
      [ show waspVersion,
        "",
        "If you wish to install/switch to different version of Wasp, do:",
        "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v x.y.z",
        "where x.y.z is the desired version.",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, include the latest one."
      ]

-- TODO(matija): maybe extract to a separate module, e.g. DbCli.hs?
dbCli :: [String] -> IO ()
dbCli args = case args of
  ["migrate-dev"] -> runDbCommand $ Command.Db.Migrate.migrateDev Nothing
  "migrate-dev" : migrateArgs -> runDbCommand $ Command.Db.Migrate.migrateDev $ Just migrateArgs
  ["studio"] -> runDbCommand studio
  _ -> printDbUsage

printDbUsage :: IO ()
printDbUsage =
  putStrLn $
    unlines
      [ title "USAGE",
        "  wasp db <command> [command-args]",
        "",
        title "COMMANDS",
        cmd
          ( "  migrate-dev     Ensures dev database corresponds to the current state of schema(entities):\n"
              <> "                  - Generates a new migration if there are changes in the schema.\n"
              <> "                  - Applies any pending migrations to the database either using the supplied migration name or asking for one.\n"
              <> "\nOPTIONS:\n"
              <> " --name [migration-name]\n"
              <> " --create-only\n"
          ),
        cmd "  studio          GUI for inspecting your database.",
        "",
        title "EXAMPLES",
        "  wasp db migrate-dev",
        "  wasp db migrate-dev --name \"Added User entity\"",
        "  wasp db migrate-dev --create-only",
        "  wasp db studio"
      ]

cmd :: String -> String
cmd = mapFirstWord (Term.applyStyles [Term.Yellow, Term.Bold])

mapFirstWord :: (String -> String) -> String -> String
mapFirstWord f s = beforeFirstWord ++ f firstWord ++ afterFirstWord
  where
    (beforeFirstWord, firstWordAndAfter) = span isSpace s
    (firstWord, afterFirstWord) = break isSpace firstWordAndAfter
