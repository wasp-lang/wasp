{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<**>), (<|>), many)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (intercalate)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import qualified System.Environment as Env
import System.Exit (exitFailure)
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.BashCompletion (bashCompletion, printBashCompletionInstruction)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.BuildStart (buildStart)
import Wasp.Cli.Command.Call (Call)
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import qualified Wasp.Cli.Command.CreateNewProject.AI as Command.CreateNewProject.AI
import Wasp.Cli.Command.CreateNewProject.AI.ArgumentsParser (newProjectAiParserInfo)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (newProjectArgsParserInfo)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (availableStarterTemplates)
import Wasp.Cli.Command.Db (runCommandThatRequiresDbRunning)
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
import Wasp.Cli.Command.Studio (studio)
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Test (test)
import Wasp.Cli.Command.TsConfigSetup (tsConfigSetup)
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
  commandCall <-
    Opt.customExecParser
      (Opt.prefs (Opt.showHelpOnError <> Opt.showHelpOnEmpty <> Opt.subparserInline))
      (Opt.info (mainParser <**> Opt.helper) mainInfo)
  
  runWaspCommand commandCall
  where
    mainInfo =
      Opt.fullDesc
        <> Opt.header "wasp - Wasp CLI tool"
        <> Opt.progDesc "A modern web app framework that makes building full-stack React & Node.js apps super easy"
        <> Opt.footer "For more information, visit https://wasp-lang.dev"
    
    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = do
      putStrLn $ "\nInternal Wasp error (bug in the compiler):\n" ++ indent 2 (show e)
      exitFailure

mainParser :: Opt.Parser Call
mainParser =
  Opt.hsubparser $
    mconcat
      [ Opt.command "new" $ Command.Call.New <$> newProjectArgsParserInfo,
        Opt.command "new:ai" $ Command.Call.NewAi <$> newProjectAiParserInfo,
        Opt.command "start" $ Opt.info startParser (Opt.progDesc "Run Wasp app in development mode"),
        Opt.command "clean" $ Opt.info (pure Command.Call.Clean) (Opt.progDesc "Delete all generated code and cached artifacts"),
        Opt.command "compile" $ Opt.info (pure Command.Call.Compile) (Opt.progDesc "Compile the Wasp app"),
        Opt.command "db" $ Opt.info (Command.Call.Db <$> dbArgsParser) (Opt.progDesc "Execute database commands"),
        Opt.command "build" $ Opt.info buildParser (Opt.progDesc "Generate full web app code, ready for deployment"),
        Opt.command "deploy" $ Opt.info (Command.Call.Deploy <$> deployArgsParser) (Opt.progDesc "Deploy your Wasp app to cloud hosting providers"),
        Opt.command "telemetry" $ Opt.info (pure Command.Call.Telemetry) (Opt.progDesc "Print telemetry status"),
        Opt.command "deps" $ Opt.info (pure Command.Call.Deps) (Opt.progDesc "Print the dependencies that Wasp uses"),
        Opt.command "dockerfile" $ Opt.info (pure Command.Call.Dockerfile) (Opt.progDesc "Print the Wasp generated Dockerfile"),
        Opt.command "info" $ Opt.info (pure Command.Call.Info) (Opt.progDesc "Print basic information about current project"),
        Opt.command "studio" $ Opt.info (pure Command.Call.Studio) (Opt.progDesc "(experimental) GUI for inspecting your Wasp app"),
        Opt.command "test" $ Opt.info (Command.Call.Test <$> testArgsParser) (Opt.progDesc "Execute tests in your project"),
        Opt.command "uninstall" $ Opt.info (pure Command.Call.Uninstall) (Opt.progDesc "Remove Wasp from your system"),
        Opt.command "ts-setup" $ Opt.info (pure Command.Call.TsSetup) (Opt.progDesc "Setup TypeScript configuration"),
        Opt.command "version" $ Opt.info (pure Command.Call.Version) (Opt.progDesc "Print current version of CLI"),
        Opt.command "completion" $ Opt.info (pure Command.Call.PrintBashCompletionInstruction) (Opt.progDesc "Print help on bash completion"),
        Opt.command "completion:list" $ Opt.info (pure Command.Call.BashCompletionListCommands) (Opt.progDesc "List commands for bash completion"),
        Opt.command "waspls" $ Opt.info (pure Command.Call.WaspLS) (Opt.progDesc "Run Wasp Language Server")
      ]

-- Parser for start command (can have "db" subcommand)
startParser :: Opt.Parser Call
startParser =
  Opt.subparser
    ( Opt.command "db" (Opt.info startDbParser (Opt.progDesc "Start managed development database"))
    )
    <|> pure Command.Call.Start
  where
    startDbParser = 
      Command.Call.StartDb . maybe [] (\img -> ["--db-image", img])
        <$> Opt.optional (Opt.strOption $
          Opt.long "db-image"
            <> Opt.metavar "IMAGE"
            <> Opt.help "Specify a custom Docker image for the database")

-- Parser for build command (can have "start" subcommand)
buildParser :: Opt.Parser Call
buildParser =
  Opt.subparser
    ( Opt.command "start" (Opt.info buildStartParser (Opt.progDesc "Preview the built production app locally"))
    )
    <|> pure Command.Call.Build
  where
    buildStartParser = Command.Call.BuildStart <$> many (Opt.strArgument (Opt.metavar "ARGS..."))

-- Parser for db subcommands
dbArgsParser :: Opt.Parser [String]
dbArgsParser =
  Opt.subparser $
    mconcat
      [ Opt.command "start" $ Opt.info startDbArgsParser (Opt.progDesc "Start managed development database"),
        Opt.command "reset" $ Opt.info (pure ["reset"]) (Opt.progDesc "Reset development database"),
        Opt.command "migrate-dev" $ Opt.info migrateDevArgsParser (Opt.progDesc "Synchronize database with the current state of schema"),
        Opt.command "seed" $ Opt.info seedArgsParser (Opt.progDesc "Execute a database seed function"),
        Opt.command "studio" $ Opt.info (pure ["studio"]) (Opt.progDesc "GUI for inspecting your database")
      ]
  where
    startDbArgsParser :: Opt.Parser [String]
    startDbArgsParser = 
      (\maybeDbImage -> case maybeDbImage of
        Nothing -> ["start"]
        Just image -> ["start", "--db-image", image])
      <$> Opt.optional (Opt.strOption $
          Opt.long "db-image"
            <> Opt.metavar "IMAGE"
            <> Opt.help "Specify a custom Docker image for the database")
    
    migrateDevArgsParser :: Opt.Parser [String]
    migrateDevArgsParser = 
      (\maybeName createOnly -> "migrate-dev" : concat
        [ maybe [] (\n -> ["--name", n]) maybeName
        , ["--create-only" | createOnly]
        ])
      <$> Opt.optional (Opt.strOption $
          Opt.long "name"
            <> Opt.metavar "MIGRATION_NAME"
            <> Opt.help "Name for the migration")
      <*> Opt.switch (
          Opt.long "create-only"
            <> Opt.help "Create migration without applying it")
    
    seedArgsParser :: Opt.Parser [String]
    seedArgsParser = 
      (\maybeSeedName -> case maybeSeedName of
        Nothing -> ["seed"]
        Just name -> ["seed", name])
      <$> Opt.optional (Opt.strArgument $ 
        Opt.metavar "SEED_NAME"
          <> Opt.help "Name of the seed to execute")

-- Parser for deploy arguments (passthrough)
deployArgsParser :: Opt.Parser [String]
deployArgsParser = many $ Opt.strArgument $ 
  Opt.metavar "DEPLOY_ARGS..."
    <> Opt.help "Arguments to pass to the deploy command"

-- Parser for test arguments
testArgsParser :: Opt.Parser [String]
testArgsParser = many $ Opt.strArgument $
  Opt.metavar "TEST_ARGS..."
    <> Opt.help "Arguments to pass to the test command"

runWaspCommand :: Call -> IO ()
runWaspCommand commandCall = do

  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall

  -- Before calling any command, check that the node requirement is met. Node is
  -- not needed for every command, but checking for every command was decided
  -- to be more robust than trying to only check for commands that require it.
  -- See https://github.com/wasp-lang/wasp/issues/1134#issuecomment-1554065668
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> do
      cliSendMessage $ Message.Failure "Node/NPM requirement not met" errorMsg
      exitFailure
    NodeVersion.VersionCheckSuccess -> pure ()

  setDefaultCliEnvVars

  case commandCall of
    Command.Call.New newArgs -> runCommand $ createNewProject newArgs
    Command.Call.NewAi newAiArgs ->
      runCommand $
        Command.CreateNewProject.AI.createNewProjectNonInteractive newAiArgs
    Command.Call.Start -> runCommand start
    Command.Call.StartDb startDbArgs -> runCommand $ Command.Start.Db.start startDbArgs
    Command.Call.Clean -> runCommand clean
    Command.Call.TsSetup -> runCommand tsConfigSetup
    Command.Call.Compile -> runCommand compile
    Command.Call.Db dbArgs -> dbCli dbArgs
    Command.Call.Version -> printVersion
    Command.Call.Studio -> runCommand studio
    Command.Call.Uninstall -> runCommand uninstall
    Command.Call.Build -> runCommand build
    Command.Call.BuildStart buildStartArgs -> runCommand $ buildStart buildStartArgs
    Command.Call.Telemetry -> runCommand Telemetry.telemetry
    Command.Call.Deps -> runCommand deps
    Command.Call.Dockerfile -> runCommand printDockerfile
    Command.Call.Info -> runCommand info
    Command.Call.PrintBashCompletionInstruction -> runCommand printBashCompletionInstruction
    Command.Call.BashCompletionListCommands -> runCommand bashCompletion
    Command.Call.WaspLS -> runWaspLS
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
        cmd   "    new:ai <app-name> <app-description> [<config-json>]",
              "      Uses AI to create a new Wasp project just based on the app name and the description.",
              "      You can do the same thing with `wasp new` interactively.",
              "      Run `wasp new:ai` for more info.",
              "",
        cmd   "    version               Prints current version of CLI.",
        cmd   "    waspls                Run Wasp Language Server. Add --help to get more info.",
        cmd   "    completion            Prints help on bash completion.",
        cmd   "    uninstall             Removes Wasp from your system.",
        title "  IN PROJECT",
        cmd   "    start                 Runs Wasp app in development mode, watching for file changes.",
        cmd   "    start db [--db-image <image>]",
              "                          Starts managed development database for you.",
              "                          Optionally specify a custom Docker image.",
        cmd   "    db <db-cmd> [args]    Executes a database command. Run 'wasp db' for more info.",
        cmd   "    clean                 Deletes all generated code, all cached artifacts, and the node_modules dir.",
              "                          Wasp equivalent of 'have you tried closing and opening it again?'.",
        cmd   "    build                 Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        cmd   "    build start [args]    Previews the built production app locally.",
        cmd   "    deploy                Deploys your Wasp app to cloud hosting providers.",
        cmd   "    telemetry             Prints telemetry status.",
        cmd   "    deps                  Prints the dependencies that Wasp uses in your project.",
        cmd   "    dockerfile            Prints the contents of the Wasp generated Dockerfile.",
        cmd   "    info                  Prints basic information about the current Wasp project.",
        cmd   "    test                  Executes tests in your project.",
        cmd   "    studio                (experimental) GUI for inspecting your Wasp app.",
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
  putStrLn $
    unlines
      [ show waspVersion,
        "",
        "If you wish to install/switch to the latest version of Wasp, do:",
        "  curl -sSL https://get.wasp.sh/installer.sh | sh -s",
        "",
        "If you want specific x.y.z version of Wasp, do:",
        "  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z",
        "",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
      ]

-- TODO: maybe extract to a separate module, e.g. DbCli.hs?
dbCli :: [String] -> IO ()
dbCli args = case args of
  -- These commands don't require an existing and running database.
  "start" : optionalStartArgs -> runCommand $ Command.Start.Db.start optionalStartArgs
  -- These commands require an existing and running database.
  ["reset"] -> runCommandThatRequiresDbRunning Command.Db.Reset.reset
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
              "  start [--db-image <image>]   Alias for `wasp start db`.",
              "                               Starts managed development database for you.",
              "                               Optionally specify a custom Docker image."
        ],
        cmd   "  reset                        Drops all data and tables from development database and re-applies all migrations.",
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