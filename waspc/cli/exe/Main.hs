module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (intercalate)
import Main.Utf8 (withUtf8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.BashCompletion (bashCompletion, generateBashCompletionScript, printBashCompletionInstruction)
import Wasp.Cli.Command.Build (build)
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import Wasp.Cli.Command.Db (runDbCommand)
import qualified Wasp.Cli.Command.Db.Migrate as Command.Db.Migrate
import qualified Wasp.Cli.Command.Db.Reset as Command.Db.Reset
import qualified Wasp.Cli.Command.Db.Seed as Command.Db.Seed
import qualified Wasp.Cli.Command.Db.Studio as Command.Db.Studio
import Wasp.Cli.Command.Deploy (deploy)
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Start.Db as Command.Start.Db
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Test (test)
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Message as Message
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Util (indent)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  args <- getArgs
  let commandCall = case args of
        ("new" : newArgs) -> Command.Call.New newArgs
        ["start"] -> Command.Call.Start
        ["start", "db"] -> Command.Call.StartDb
        ["clean"] -> Command.Call.Clean
        ["compile"] -> Command.Call.Compile
        ("db" : dbArgs) -> Command.Call.Db dbArgs
        ["uninstall"] -> Command.Call.Uninstall
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
        ("deploy" : deployArgs) -> Command.Call.Deploy deployArgs
        ("test" : testArgs) -> Command.Call.Test testArgs
        _ -> Command.Call.Unknown args

  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall

  -- Before calling any command, check that the node requirement is met. Node is
  -- not needed for every command, but checking for every command was decided
  -- to be more robust than trying to only check for commands that require it.
  -- See https://github.com/wasp-lang/wasp/issues/1134#issuecomment-1554065668
  NodeVersion.getAndCheckNodeVersion >>= \case
    Left errorMsg -> do
      cliSendMessage $ Message.Failure "Node requirement not met" errorMsg
      exitFailure
    Right _ -> pure ()

  case commandCall of
    Command.Call.New newArgs -> runCommand $ createNewProject newArgs
    Command.Call.Start -> runCommand start
    Command.Call.StartDb -> runCommand Command.Start.Db.start
    Command.Call.Clean -> runCommand clean
    Command.Call.Compile -> runCommand compile
    Command.Call.Db dbArgs -> dbCli dbArgs
    Command.Call.Version -> printVersion
    Command.Call.Uninstall -> runCommand uninstall
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
    Command.Call.Deploy deployArgs -> runCommand $ deploy deployArgs
    Command.Call.Test testArgs -> runCommand $ test testArgs

  -- If sending of telemetry data is still not done 1 second since commmand finished, abort it.
  -- We also make sure here to catch all errors that might get thrown and silence them.
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = do
      putStrLn $ "\nInternal Wasp error (bug in compiler):\n" ++ indent 2 (show e)
      exitFailure

{- ORMOLU_DISABLE -}
printUsage :: IO ()
printUsage =
  putStrLn $
    unlines
      [ title "USAGE",
              "  wasp <command> [command-args]",
              "",
        title "COMMANDS",
        title "  GENERAL",
        cmd   "    new [<name>] [args]   Creates a new Wasp project. Run it without arguments for interactive mode.",
              "      OPTIONS:",
              "        -t|--template <template-name>",
              "           Check out the templates list here: https://github.com/wasp-lang/starters",
              "",
        cmd   "    version               Prints current version of CLI.",
        cmd   "    waspls                Run Wasp Language Server. Add --help to get more info.",
        cmd   "    completion            Prints help on bash completion.",
        cmd   "    uninstall             Removes Wasp from your system.",
        title "  IN PROJECT",
        cmd   "    start                 Runs Wasp app in development mode, watching for file changes.",
        cmd   "    start db              Starts managed development database for you.",
        cmd   "    db <db-cmd> [args]    Executes a database command. Run 'wasp db' for more info.",
        cmd   "    clean                 Deletes all generated code and other cached artifacts.",
              "                          Wasp equivalent of 'have you tried closing and opening it again?'.",
        cmd   "    build                 Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        cmd   "    deploy                Deploys your Wasp app to cloud hosting providers.",
        cmd   "    telemetry             Prints telemetry status.",
        cmd   "    deps                  Prints the dependencies that Wasp uses in your project.",
        cmd   "    dockerfile            Prints the contents of the Wasp generated Dockerfile.",
        cmd   "    info                  Prints basic information about current Wasp project.",
        cmd   "    test                  Executes tests in your project.",
              "",
        title "EXAMPLES",
              "  wasp new MyApp",
              "  wasp start",
              "  wasp db migrate-dev",
              "",
        Term.applyStyles [Term.Green]   "Docs:" ++ " https://wasp-lang.dev/docs",
        Term.applyStyles [Term.Magenta] "Discord (chat):" ++ " https://discord.gg/rzdnErX",
        Term.applyStyles [Term.Cyan]    "Newsletter:" ++ " https://wasp-lang.dev/#signup"
      ]
{- ORMOLU_ENABLE -}

printVersion :: IO ()
printVersion = do
  putStrLn $
    unlines
      [ show waspVersion,
        "",
        "If you wish to install/switch to the latest version of Wasp, do:",
        "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s",
        "",
        "If you want specific x.y.z version of Wasp, do:",
        "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v x.y.z",
        "",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
      ]

-- TODO(matija): maybe extract to a separate module, e.g. DbCli.hs?
dbCli :: [String] -> IO ()
dbCli args = case args of
  ["start"] -> runCommand Command.Start.Db.start
  "migrate-dev" : optionalMigrateArgs -> runDbCommand $ Command.Db.Migrate.migrateDev optionalMigrateArgs
  ["reset"] -> runDbCommand Command.Db.Reset.reset
  ["seed"] -> runDbCommand $ Command.Db.Seed.seed Nothing
  ["seed", seedName] -> runDbCommand $ Command.Db.Seed.seed $ Just seedName
  ["studio"] -> runDbCommand Command.Db.Studio.studio
  _ -> printDbUsage

{- ORMOLU_DISABLE -}
printDbUsage :: IO ()
printDbUsage =
  putStrLn $
    unlines
      [ title "USAGE",
              "  wasp db <command> [command-args]",
              "",
        title "COMMANDS",
        cmd   "  start         Alias for `wasp start db`.",
        cmd   "  reset         Drops all data and tables from development database and re-applies all migrations.",
        cmd   "  seed [name]   Executes a db seed function (specified via app.db.seeds).",
              "                If there are multiple seeds, you can specify a seed to execute by providing its name,",
              "                or if not then you will be asked to provide the name interactively.",
        cmd $ intercalate "\n" [
              "  migrate-dev   Ensures dev database corresponds to the current state of schema(entities):",
              "                  - Generates a new migration if there are changes in the schema.",
              "                  - Applies any pending migrations to the database either using the",
              "                    supplied migration name or asking for one.",
              "    OPTIONS:",
              "      --name [migration-name]",
              "      --create-only"
        ],
        cmd   "  studio        GUI for inspecting your database.",
              "",
        title "EXAMPLES",
              "  wasp db migrate-dev",
              "  wasp db migrate-dev --name \"Added User entity\"",
              "  wasp db migrate-dev --create-only",
              "  wasp db studio"
      ]
{- ORMOLU_ENABLE -}

cmd :: String -> String
cmd = mapFirstWord (Term.applyStyles [Term.Yellow, Term.Bold])

mapFirstWord :: (String -> String) -> String -> String
mapFirstWord f s = beforeFirstWord ++ f firstWord ++ afterFirstWord
  where
    (beforeFirstWord, firstWordAndAfter) = span isSpace s
    (firstWord, afterFirstWord) = break isSpace firstWordAndAfter
