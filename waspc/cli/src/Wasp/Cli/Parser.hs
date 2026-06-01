module Wasp.Cli.Parser
  ( Action (..),
    actionToCall,
    parseAction,
    topLevelParserInfo,
  )
where

import Control.Applicative ((<|>))
import Data.List (intersperse)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Pretty as Opt.Doc
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs, buildStartArgsParser)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.CreateNewProject.AI (NewAiArgs, newAiArgsParser)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs, newProjectArgsParser)
import Wasp.Cli.Command.Db.Migrate (migrateArgsParser)
import Wasp.Cli.Command.Db.Reset (resetArgsParser)
import Wasp.Cli.Command.Db.Seed (seedArgsParser)
import Wasp.Cli.Command.Start.Db (StartDbArgs, startDbArgsParser)
import Wasp.Cli.Command.Test (TestArgs, testArgsParser)
import Wasp.Cli.Command.WaspLS (WaspLSArgs, waspLSArgsParser)
import Wasp.Cli.Terminal (title)
import Wasp.Generator.DbGenerator.Common (MigrateArgs, ResetArgs)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

-- | An invocation of the CLI parsed from the command line. Each constructor
-- corresponds to a leaf subcommand, holding the arguments already parsed and
-- validated by optparse-applicative.
data Action
  = ANew NewProjectArgs
  | ANewAi NewAiArgs
  | AStart
  | AStartDb StartDbArgs
  | ADbStart StartDbArgs
  | ADbReset ResetArgs
  | ADbMigrateDev MigrateArgs
  | ADbSeed (Maybe String)
  | ADbStudio
  | AClean
  | AInstall
  | AUninstall
  | ACompile
  | ABuild
  | ABuildStart BuildStartArgs
  | AVersion
  | ATelemetry
  | ADeps
  | ADockerfile
  | AInfo
  | ANews
  | AStudio
  | APrintBashCompletionInstruction
  | ABashCompletionListCommands
  | AWaspLS WaspLSArgs
  | ADeploy [String]
  | ATest TestArgs

-- | Maps an 'Action' to the coarse classification telemetry needs.
actionToCall :: Action -> Call.Call
actionToCall ABuild = Call.Build
actionToCall (ADeploy args) = Call.Deploy args
actionToCall _ = Call.Other

parseAction :: IO Action
parseAction = Opt.customExecParser parserPrefs topLevelParserInfo

parserPrefs :: Opt.ParserPrefs
parserPrefs =
  Opt.prefs $
    Opt.showHelpOnEmpty
      <> Opt.showHelpOnError

topLevelParserInfo :: Opt.ParserInfo Action
topLevelParserInfo =
  Opt.info
    (commandsParser Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Compiles .wasp configuration files into React + Node.js full-stack web apps."
        <> Opt.header ("wasp v" <> show waspVersion)
        <> Opt.footerDoc (Just helpFooterDoc)
    )

-- | Examples and the (color-coded) community links shown at the bottom of the
-- top-level help. optparse-applicative's plain-text 'Opt.footer' reflows
-- whitespace and would drop both the line breaks and our ANSI styling, so we
-- build a 'Opt.Doc.Doc' with hard line breaks and embed the styled strings
-- (raw ANSI escapes from 'Term.applyStyles', as used elsewhere in the CLI).
helpFooterDoc :: Opt.Doc.Doc
helpFooterDoc =
  mconcat . intersperse Opt.Doc.hardline . map Opt.Doc.pretty $
    [ title "EXAMPLES",
      "  wasp new MyApp",
      "  wasp start",
      "  wasp db migrate-dev",
      "",
      Term.applyStyles [Term.Green] "Docs:" <> " https://wasp.sh/docs",
      Term.applyStyles [Term.Magenta] "Discord (chat):" <> " https://discord.gg/rzdnErX",
      Term.applyStyles [Term.Cyan] "Newsletter:" <> " https://wasp.sh/#signup"
    ]

commandsParser :: Opt.Parser Action
commandsParser =
  Opt.hsubparser $
    mconcat
      [ -- General
        sub "new" "Create a new Wasp project. Run without arguments for interactive mode." (ANew <$> newProjectArgsParser),
        sub
          "new:ai"
          "Use AI to create a new Wasp project from a short description."
          (ANewAi <$> newAiArgsParser),
        sub "version" "Print the version of the CLI." (pure AVersion),
        subInfo
          "waspls"
          (AWaspLS <$> waspLSArgsParser)
          (Opt.progDesc "Run the Wasp Language Server."),
        sub "completion" "Print bash auto-completion install instructions." (pure APrintBashCompletionInstruction),
        sub "completion:list" "List commands for bash completion (used by complete -C)." (pure ABashCompletionListCommands),
        sub "uninstall" "Remove Wasp from your system." (pure AUninstall),
        -- In project
        subInfo
          "start"
          startParser
          (Opt.progDesc "Run the Wasp app in development mode, watching for file changes."),
        subInfo
          "db"
          dbParser
          (Opt.progDesc "Execute a database command. Run 'wasp db --help' for details."),
        sub "install" "Set up all internal Wasp npm dependencies and run npm install." (pure AInstall),
        sub
          "clean"
          "Delete the generated app, cached artifacts, and node_modules. Wasp's \"try turning it off and on again\"."
          (pure AClean),
        subInfo
          "build"
          buildParser
          (Opt.progDesc "Generate the full web app, ready for deployment."),
        subInfo
          "deploy"
          deployParser
          (Opt.progDesc "Deploy your Wasp app to cloud hosting providers." <> Opt.forwardOptions),
        sub "telemetry" "Print telemetry status." (pure ATelemetry),
        sub "deps" "Print the dependencies that Wasp uses in your project." (pure ADeps),
        sub "dockerfile" "Print the contents of the Wasp-generated Dockerfile." (pure ADockerfile),
        sub "info" "Print basic information about the current Wasp project." (pure AInfo),
        subInfo
          "test"
          (ATest <$> testArgsParser)
          (Opt.progDesc "Run tests in your project."),
        sub "studio" "(experimental) GUI for inspecting your Wasp app." (pure AStudio),
        sub "news" "Read the latest Wasp-related news." (pure ANews),
        -- Hidden internal command, only used by the top-level dispatcher.
        Opt.command "compile" (Opt.info (pure ACompile) (Opt.progDesc "(internal) Run the Wasp compiler without starting the app." <> Opt.fullDesc))
      ]
  where
    sub :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
    sub name desc parser =
      Opt.command name (Opt.info parser (Opt.progDesc desc))

    subInfo :: String -> Opt.Parser a -> Opt.InfoMod a -> Opt.Mod Opt.CommandFields a
    subInfo name parser infoMod =
      Opt.command name (Opt.info parser infoMod)

-- | `wasp start` runs the dev server; `wasp start db [...]` is an alias for
-- `wasp db start [...]`.
startParser :: Opt.Parser Action
startParser =
  Opt.hsubparser
    ( Opt.command
        "db"
        ( Opt.info
            (AStartDb <$> startDbArgsParser)
            (Opt.progDesc "Start the managed development database for this Wasp project.")
        )
    )
    <|> pure AStart

-- | `wasp build` produces the deployable bundle; `wasp build start [...]` runs it locally.
buildParser :: Opt.Parser Action
buildParser =
  Opt.hsubparser
    ( Opt.command
        "start"
        ( Opt.info
            (ABuildStart <$> buildStartArgsParser)
            (Opt.progDesc "Preview the built production app locally.")
        )
    )
    <|> pure ABuild

dbParser :: Opt.Parser Action
dbParser =
  Opt.hsubparser $
    mconcat
      [ Opt.command
          "start"
          ( Opt.info
              (ADbStart <$> startDbArgsParser)
              (Opt.progDesc "Alias for `wasp start db`.")
          ),
        Opt.command
          "reset"
          ( Opt.info
              (ADbReset <$> resetArgsParser)
              (Opt.progDesc "Drop all data and tables from the dev database and re-apply all migrations.")
          ),
        Opt.command
          "seed"
          ( Opt.info
              (ADbSeed <$> seedArgsParser)
              (Opt.progDesc "Run a database seed function (defined via app.db.seeds).")
          ),
        Opt.command
          "migrate-dev"
          ( Opt.info
              (ADbMigrateDev <$> migrateArgsParser)
              ( Opt.progDesc
                  ( "Ensure the dev database matches the current schema: generate a migration if the"
                      <> " schema has changed and apply any pending migrations."
                  )
              )
          ),
        Opt.command
          "studio"
          ( Opt.info
              (pure ADbStudio)
              (Opt.progDesc "Open a GUI for inspecting the database.")
          )
      ]

deployParser :: Opt.Parser Action
deployParser =
  ADeploy
    <$> Opt.many
      ( Opt.strArgument $
          Opt.metavar "DEPLOY_ARGS..."
            <> Opt.help "Arguments passed through to the deploy script (e.g. `fly setup my-app mia`)"
      )
