module Wasp.Cli.Parser
  ( parse,
    topLevelParserInfo,
  )
where

import Data.List (intersperse)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Pretty as Opt.Doc
import qualified Wasp.Cli.Command.BashCompletion as BashCompletion
import qualified Wasp.Cli.Command.Build as Build
import qualified Wasp.Cli.Command.Clean as Clean
import qualified Wasp.Cli.Command.Compile as Compile
import qualified Wasp.Cli.Command.CreateNewProject as CreateNewProject
import qualified Wasp.Cli.Command.CreateNewProject.AI as CreateNewProject.AI
import qualified Wasp.Cli.Command.Db as Db
import Wasp.Cli.Command.Definition (subcommandsParser)
import qualified Wasp.Cli.Command.Deploy as Deploy
import qualified Wasp.Cli.Command.Deps as Deps
import qualified Wasp.Cli.Command.Dockerfile as Dockerfile
import qualified Wasp.Cli.Command.Info as Info
import qualified Wasp.Cli.Command.Install as Install
import qualified Wasp.Cli.Command.News as News
import qualified Wasp.Cli.Command.Start as Start
import qualified Wasp.Cli.Command.Studio as Studio
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import qualified Wasp.Cli.Command.Test as Test
import qualified Wasp.Cli.Command.Uninstall as Uninstall
import qualified Wasp.Cli.Command.Version as Version
import qualified Wasp.Cli.Command.WaspLS as WaspLS
import Wasp.Cli.Terminal (title)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

-- | Parses the command line into the IO action that should be run. Handles
-- @--help@, parse errors, and exiting on its own (never returns in those cases).
parse :: IO (IO ())
parse = Opt.customExecParser parserPrefs topLevelParserInfo

parserPrefs :: Opt.ParserPrefs
parserPrefs =
  Opt.prefs $
    Opt.showHelpOnEmpty
      <> Opt.showHelpOnError

-- | The whole CLI as a single parser. Each command owns its own parser
-- (a @ParserInfo (IO ())@), so this module is just the registry that maps
-- command names to those parsers and adds the top-level description/footer.
topLevelParserInfo :: Opt.ParserInfo (IO ())
topLevelParserInfo =
  Opt.info
    (commandsParser Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Compiles .wasp configuration files into React + Node.js full-stack web apps."
        <> Opt.header ("wasp v" <> show waspVersion)
        <> Opt.footerDoc (Just helpFooterDoc)
    )

commandsParser :: Opt.Parser (IO ())
commandsParser =
  subcommandsParser
    [ -- General
      ("new", CreateNewProject.parserInfo),
      ("new:ai", CreateNewProject.AI.parserInfo),
      ("version", Version.parserInfo),
      ("waspls", WaspLS.parserInfo),
      ("completion", BashCompletion.printInstructionParserInfo),
      ("completion:list", BashCompletion.listCommandsParserInfo),
      ("uninstall", Uninstall.parserInfo),
      -- In project
      ("start", Start.parserInfo),
      ("db", Db.parserInfo),
      ("install", Install.parserInfo),
      ("clean", Clean.parserInfo),
      ("build", Build.parserInfo),
      ("deploy", Deploy.parserInfo),
      ("telemetry", Telemetry.parserInfo),
      ("deps", Deps.parserInfo),
      ("dockerfile", Dockerfile.parserInfo),
      ("info", Info.parserInfo),
      ("test", Test.parserInfo),
      ("studio", Studio.parserInfo),
      ("news", News.parserInfo),
      -- Hidden internal command, used by the top-level dispatcher.
      ("compile", Compile.parserInfo)
    ]

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
