module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (intercalate)
import Main.Utf8 (withUtf8)
import System.Environment (getArgs)
import qualified System.Environment as Env
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.BashCompletion (bashCompletion, printBashCompletionInstruction)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.BuildStart (buildStart)
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (availableStarterTemplates)
import Wasp.Cli.Command.Db (runCommandThatRequiresDbRunning)
import qualified Wasp.Cli.Command.Db.Migrate as Command.Db.Migrate
import qualified Wasp.Cli.Command.Db.Reset as Command.Db.Reset
import qualified Wasp.Cli.Command.Db.Seed as Command.Db.Seed
import qualified Wasp.Cli.Command.Db.Studio as Command.Db.Studio
import Wasp.Cli.Command.Deploy (deploy)
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Doctor (doctor)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Install (install)
import Wasp.Cli.Command.News (news)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Start.Db as Command.Start.Db
import Wasp.Cli.Command.Studio (studio)
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Test (test)
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Terminal (title)
import Wasp.Util (indent)
import Wasp.Util.InstallMethod (getInstallationCommand)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  args <- getArgs
  let commandCall = case args of
        ("new" : newArgs) -> Command.Call.New newArgs
        ["start"] -> Command.Call.Start
        ("start" : "db" : startDbArgs) -> Command.Call.StartDb startDbArgs
        ["clean"] -> Command.Call.Clean
        ["install"] -> Command.Call.Install
        ["compile"] -> Command.Call.Compile
        ("db" : dbArgs) -> Command.Call.Db dbArgs
        ["uninstall"] -> Command.Call.Uninstall
        ["version"] -> Command.Call.Version
        ["doctor"] -> Command.Call.Doctor
        ["build"] -> Command.Call.Build
        ("build" : "start" : buildStartArgs) -> Command.Call.BuildStart buildStartArgs
        ["telemetry"] -> Command.Call.Telemetry
        ["deps"] -> Command.Call.Deps
        ["dockerfile"] -> Command.Call.Dockerfile
        ["info"] -> Command.Call.Info
        ["news"] -> Command.Call.News
        ["studio"] -> Command.Call.Studio
        ["completion"] -> Command.Call.PrintBashCompletionInstruction
        ["completion:list"] -> Command.Call.BashCompletionListCommands
        ("deploy" : deployArgs) -> Command.Call.Deploy deployArgs
        ("test" : testArgs) -> Command.Call.Test testArgs
        _unknownCommand -> Command.Call.Unknown args

  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall

  setDefaultCliEnvVars

  case commandCall of
    Command.Call.New newArgs -> runCommand $ createNewProject newArgs
    Command.Call.Start -> runCommand start
    Command.Call.StartDb startDbArgs -> runCommand $ Command.Start.Db.start startDbArgs
    Command.Call.Clean -> runCommand clean
    Command.Call.Install -> runCommand install
    Command.Call.Compile -> runCommand compile
    Command.Call.Db dbArgs -> dbCli dbArgs
    Command.Call.Version -> printVersion
    Command.Call.Doctor -> doctor
    Command.Call.Studio -> runCommand studio
    Command.Call.Uninstall -> runCommand uninstall
    Command.Call.Build -> runCommand build
    Command.Call.BuildStart buildStartArgs -> runCommand $ buildStart buildStartArgs
    Command.Call.Telemetry -> runCommand Telemetry.telemetry
    Command.Call.Deps -> runCommand deps
    Command.Call.Dockerfile -> runCommand printDockerfile
    Command.Call.Info -> runCommand info
    Command.Call.News -> runCommand news
    Command.Call.PrintBashCompletionInstruction -> runCommand printBashCompletionInstruction
    Command.Call.BashCompletionListCommands -> runCommand bashCompletion
    Command.Call.Deploy deployArgs -> runCommand $ deploy deployArgs
    Command.Call.Test testArgs -> runCommand $ test testArgs
    Command.Call.Unknown _ -> printUsage >> exitFailure
  -- If sending of telemetry data is still not done 1 second since commmand finished, abort it.
  -- We also make sure here to catch all errors that might get thrown and silence them.
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = do
      putStrLn $ "\nInternal Wasp error (bug in the compiler):\n" ++ indent 2 (show e)
      exitFailure

-- | Sets env variables that are visible to the commands run by the CLI.
-- For example, we can use this to hide update messages by tools like Prisma.
-- The env variables are visible to our CLI and any child processes spawned by it.
-- The env variables won't be set in the terminal session after the CLI exits.
setDefaultCliEnvVars :: IO ()
setDefaultCliEnvVars = do
  mapM_ (uncurry Env.setEnv) cliEnvVars
  where
    cliEnvVars :: [(String, String)]
    cliEnvVars =
      [ ("PRISMA_HIDE_UPDATE_MESSAGE", "true")
      ]

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
              "           Available starter templates are: " <> intercalate ", " (map show availableStarterTemplates) <> ".",
              "",
        cmd   "    version               Prints current version of CLI.",
        cmd   "    doctor                Checks your machine for Wasp requirements (Node.js, Docker, ports, ...).",
        cmd   "    completion            Prints help on bash completion.",
        cmd   "    uninstall             Removes Wasp from your system.",
        title "  IN PROJECT",
        cmd   "    start                 Runs Wasp app in development mode, watching for file changes.",
        cmd   "    start db [--db-image <image>] [--db-volume-mount-path <path>]",
              "                          Starts managed development database for you.",
              "                          Optionally specify a custom Docker image or Docker volume mount path.",
        cmd   "    db <db-cmd> [args]    Executes a database command. Run 'wasp db' for more info.",
        cmd   "    install               Sets up all internal Wasp npm dependencies and runs npm install.",
        cmd   "    clean                 Deletes the generated app, all cached artifacts, and the node_modules dir.",
              "                          Wasp equivalent of 'have you tried closing and opening it again?'.",
        cmd   "    compile               Compiles your Wasp project and reports any errors, without running it.",
        cmd   "    build                 Generates the full web app, ready for deployment.",
        cmd   "    build start [args]    Previews the built production app locally.",
        cmd   "    deploy                Deploys your Wasp app to cloud hosting providers.",
        cmd   "    telemetry             Prints telemetry status.",
        cmd   "    deps                  Prints the dependencies that Wasp uses in your project.",
        cmd   "    dockerfile            Prints the contents of the Wasp generated Dockerfile.",
        cmd   "    info                  Prints basic information about the current Wasp project.",
        cmd   "    test                  Executes tests in your project.",
        cmd   "    studio                (experimental) GUI for inspecting your Wasp app.",
        cmd   "    news                  Read the latest Wasp-related news.",
              "",
        title "EXAMPLES",
              "  wasp new MyApp",
              "  wasp start",
              "  wasp db migrate-dev",
              "",
        Term.applyStyles [Term.Green]   "Docs:" ++ " https://wasp.sh/docs",
        Term.applyStyles [Term.Magenta] "Discord (chat):" ++ " https://discord.gg/rzdnErX",
        Term.applyStyles [Term.Cyan]    "Newsletter:" ++ " https://wasp.sh/#signup"
      ]
{- ORMOLU_ENABLE -}

printVersion :: IO ()
printVersion = do
  hPutStrLn stdout $ show waspVersion
  hPutStrLn stderr $
    unlines
      [ "",
        "If you wish to install/switch to the latest version of Wasp, do:",
        indent 2 $ getInstallationCommand Nothing,
        "",
        "If you want a specific x.y.z version of Wasp, do:",
        indent 2 $ getInstallationCommand $ Just "x.y.z",
        "",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
      ]

-- TODO: maybe extract to a separate module, e.g. DbCli.hs?
dbCli :: [String] -> IO ()
dbCli args = case args of
  -- These commands don't require an existing and running database.
  "start" : optionalStartArgs -> runCommand $ Command.Start.Db.start optionalStartArgs
  -- These commands require an existing and running database.
  "reset" : resetArgs -> runCommandThatRequiresDbRunning $ Command.Db.Reset.reset resetArgs
  "migrate-dev" : optionalMigrateArgs -> runCommandThatRequiresDbRunning $ Command.Db.Migrate.migrateDev optionalMigrateArgs
  ["seed"] -> runCommandThatRequiresDbRunning $ Command.Db.Seed.seed Nothing
  ["seed", seedName] -> runCommandThatRequiresDbRunning $ Command.Db.Seed.seed $ Just seedName
  ["studio"] -> runCommandThatRequiresDbRunning Command.Db.Studio.studio
  _unknownDbCommand -> printDbUsage >> exitFailure

{- ORMOLU_DISABLE -}
printDbUsage :: IO ()
printDbUsage =
  putStrLn $
    unlines
      [ title "USAGE",
              "  wasp db <command> [command-args]",
              "",
        title "COMMANDS",
        cmd $ intercalate "\n" [
              "  start [--db-image <image>] [--db-volume-mount-path <path>]",
              "                               Alias for `wasp start db`.",
              "                               Starts managed development database for you.",
              "                               Optionally specify a custom Docker image or Docker volume mount path."
        ],
        cmd   "  reset [args]                 Drops all data and tables from development database and re-applies all migrations.",
        cmd   "  seed [name]                  Executes a db seed function (specified via app.db.seeds).",
              "                               If there are multiple seeds, you can specify a seed to execute by providing its name,",
              "                               or if not then you will be asked to provide the name interactively.",
        cmd $ intercalate "\n" [
              "  migrate-dev                  Ensures dev database corresponds to the current state of schema(entities):",
              "                                 - Generates a new migration if there are changes in the schema.",
              "                                 - Applies any pending migrations to the database either using the",
              "                                   supplied migration name or asking for one.",
              "    OPTIONS:",
              "      --name [migration-name]",
              "      --create-only"
        ],
        cmd   "  studio                       GUI for inspecting your database.",
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
