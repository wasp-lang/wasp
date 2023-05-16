module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Main.Utf8 (withUtf8)
import Options.Applicative (Applicative (liftA2), Parser, (<**>), (<|>))
import qualified Options.Applicative as O
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.Call (Call (..))
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import qualified Wasp.Cli.Command.CreateNewProject.ArgumentsParser as AP
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Start.Db as Command.Start.Db
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Util (indent)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)
import Wasp.Cli.Command.Deploy (deploy)
import Wasp.Cli.Command.Test (test)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  commandCall <- runParser
  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall
  run commandCall
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = putStrLn $ "\nInternal Wasp error (bug in compiler):\n" ++ indent 2 (show e)

run :: Call -> IO ()
run = \case
  (New args) -> undefined
  Start -> undefined
  Clean -> runCommand clean
  Uninstall -> runCommand uninstall
  Compile -> runCommand compile
  Db _ -> undefined
  Build -> runCommand build
  Version -> printVersion
  Telemetry -> runCommand Telemetry.telemetry
  Deps -> runCommand deps
  Dockerfile -> runCommand printDockerfile
  Info -> runCommand info
  WaspLS -> runWaspLS
  -- FIXME: Replace custom parsers with optparse for deploy and test commands.
  Deploy args -> runCommand $ deploy args
  Test args -> runCommand $ test args

runParser :: IO Call
runParser = do O.customExecParser p opts
  where
    opts =
      O.info
        (parserSuite <**> O.helper)
        ( O.fullDesc
            <> O.footer
              -- FIXME: Fix stdout formatting.
              ( unlines
                  [ Term.applyStyles [Term.Green] "Docs:" <> " https://wasp-lang.dev/docs",
                    Term.applyStyles [Term.Magenta] "Discord (chat):" <> " https://discord.gg/rzdnErX",
                    Term.applyStyles [Term.Cyan] "Newsletter:" <> " https://wasp-lang.dev/#signup"
                  ]
              )
        )
    p = O.prefs O.showHelpOnEmpty

mkCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkCommand name callCommand description =
  O.command name (O.info (O.helper <*> callCommand) (O.progDesc description))

generalCommands :: Parser Call
generalCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL",
        mkCommand "new" undefined "Creates a new Wasp project. Run it without arguments for interactive mode.",
        mkCommand "version" (pure Version) "Prints current version of CLI.",
        mkCommand "waspls" (pure WaspLS) "Run Wasp Language Server. Add --help to get more info.",
        mkCommand "uninstall" (pure Uninstall) "Removes Wasp from your system."
      ]


deployRestArgs :: Parser String
deployRestArgs = O.strArgument (O.metavar "DEPLOY_ARGUMENTS" <> O.help "Uncovered deploy arguments")

testRestArgs :: Parser String
testRestArgs = O.strArgument (O.metavar "TEST_ARGUMENTS" <> O.help "Uncovered test arguments")

mkWrapperCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkWrapperCommand name callCommand description =
  O.command name (O.info (O.helper <*> callCommand) (O.progDesc description <> O.noIntersperse))

inProjectCommands :: Parser Call
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        mkCommand "start" undefined "Runs Wasp app in development mode, watching for file changes.",
        mkCommand "clean" (pure Clean) "Deletes all generated code and other cached artifacts.",
        mkCommand "build" (pure Build) "Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        mkCommand "compile" (pure Compile) "--COMMAND NOT DOCUMENTED--",
        mkWrapperCommand "deploy" (Deploy <$> O.many deployRestArgs) "Deploys your Wasp app to cloud hosting providers.",
        mkCommand "deps" (pure Deps) "Prints the dependencies that Wasp uses in your project.",
        mkCommand "dockerfile" (pure Dockerfile) "Prints the contents of the Wasp generated Dockerfile.",
        mkCommand "info" (pure Info) "Prints basic information about current Wasp project.",
        mkWrapperCommand "test" (Test <$> O.many testRestArgs) "Executes tests in your project."
      ]

parserSuite :: Parser Call
parserSuite = generalCommands <|> inProjectCommands

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
