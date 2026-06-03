-- | Combinators for defining a CLI command's parser.
--
-- Each command in 'Wasp.Cli.Parser' is a @ParserInfo (IO ())@: parsing it
-- yields the action to run. These helpers build that value while hiding the
-- boilerplate every command would otherwise repeat: wrapping in 'Opt.info' with
-- a description, picking a runner ('runCommand'), and reporting telemetry.
module Wasp.Cli.Command.Definition
  ( CommandParserInfo,

    -- * Telemetry-reporting runners
    runWaspCommand,
    runWaspCommandAs,
    runWaspIO,

    -- * Leaf commands
    leaf,
    leafWithArgs,
    command,
    commandWithArgs,

    -- * Commands with subcommands
    subcommandsParser,
    commandGroup,
    commandWithSubcommands,
  )
where

import Control.Applicative ((<|>))
import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command, runCommand)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.Telemetry (runWithTelemetry)

-- | Parsing a command yields the 'IO' action to run for it.
type CommandParserInfo = Opt.ParserInfo (IO ())

-- | Run an 'IO' action, reporting generic telemetry alongside it.
runWaspIO :: IO () -> IO ()
runWaspIO = runWithTelemetry Call.Other

-- | Run a 'Command', reporting generic telemetry alongside it.
runWaspCommand :: Command a -> IO ()
runWaspCommand = runWaspIO . runCommand

-- | Run a 'Command', reporting telemetry for the given 'Call.Call'. Used by the
-- few commands telemetry distinguishes (e.g. @build@, @deploy@).
runWaspCommandAs :: Call.Call -> Command a -> IO ()
runWaspCommandAs call = runWithTelemetry call . runCommand

-- | A leaf command from a ready-made action. Escape hatch for actions that
-- aren't a plain 'Command' or use a non-default runner.
leaf :: String -> IO () -> CommandParserInfo
leaf description action = leafWithArgs description (pure action)

-- | A leaf command whose action is produced by the given parser. Escape hatch
-- for actions that aren't a plain 'Command' or use a non-default runner.
leafWithArgs :: String -> Opt.Parser (IO ()) -> CommandParserInfo
leafWithArgs description actionParser = Opt.info actionParser (Opt.progDesc description)

-- | A leaf command that takes no arguments.
command :: String -> Command a -> CommandParserInfo
command description cmd = leaf description (runWaspCommand cmd)

-- | A leaf command whose 'Command' is produced by the given parser. Build the
-- parser by applying a handler to the argument parsers, e.g.
-- @commandWithArgs "..." (handler \<$\> argP1 \<*\> argP2)@.
commandWithArgs :: String -> Opt.Parser (Command b) -> CommandParserInfo
commandWithArgs description commandParser =
  leafWithArgs description (runWaspCommand <$> commandParser)

-- | Dispatches to one of the named subcommands.
subcommandsParser :: [(String, CommandParserInfo)] -> Opt.Parser (IO ())
subcommandsParser = Opt.hsubparser . foldMap (uncurry Opt.command)

-- | A command that only dispatches to subcommands.
commandGroup :: String -> [(String, CommandParserInfo)] -> CommandParserInfo
commandGroup description subcommands =
  Opt.info (subcommandsParser subcommands) (Opt.progDesc description)

-- | A command that runs @bare@ on its own, or dispatches to a subcommand.
commandWithSubcommands :: String -> IO () -> [(String, CommandParserInfo)] -> CommandParserInfo
commandWithSubcommands description bare subcommands =
  Opt.info (subcommandsParser subcommands <|> pure bare) (Opt.progDesc description)
