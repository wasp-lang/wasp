module Wasp.Cli.Util.Parser
  ( ArgsParser (..),
    withArguments,
    getParserHelpMessage,
  )
where

import Control.Applicative ((<**>))
import Control.Monad.Except (throwError)
import qualified Options.Applicative as Opt
import qualified System.Exit as EC
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

data ArgsParser a = ArgsParser
  { commandName :: String,
    optParser :: Opt.Parser a
  }

withArguments :: ArgsParser a -> (a -> Command ()) -> Arguments -> Command ()
withArguments argsParser onSuccess args =
  case parseArguments argsParser args of
    (ArgsParsed result) -> onSuccess result
    (ParseFailure helpMessage) -> throwError $ CommandError "Parsing arguments failed" helpMessage
    (ShowHelp helpMessage) -> cliSendMessageC $ Msg.Info helpMessage

getParserHelpMessage :: ArgsParser a -> String
getParserHelpMessage argsParser =
  case parseArguments argsParser [helpFlag] of
    ShowHelp helpMessage -> helpMessage
    _unexpected -> error $ "Asking '" <> commandName argsParser <> "' for " <> helpFlag <> " didn't produce help, but this should never happen"
  where
    -- Must match the flag 'Opt.helper' defines.
    helpFlag = "--help"

data ArgsParseResult args
  = ArgsParsed args
  | ParseFailure String
  | ShowHelp String

parseArguments :: ArgsParser a -> Arguments -> ArgsParseResult a
parseArguments ArgsParser {commandName = cmdName, optParser = optParser'} args =
  case Opt.execParserPure parserPreferences parserInfo args of
    (Opt.Success success) -> ArgsParsed success
    (Opt.CompletionInvoked _) ->
      error $ "Completion invoked when parsing '" <> cmdName <> "', but this should never happen"
    (Opt.Failure failure) ->
      case Opt.execFailure failure cmdName of
        (help, EC.ExitSuccess, _) -> ShowHelp $ show help
        (help, EC.ExitFailure _, _) -> ParseFailure $ show help
  where
    parserInfo = Opt.info (optParser' <**> Opt.helper) Opt.fullDesc

parserPreferences :: Opt.ParserPrefs
parserPreferences = Opt.defaultPrefs
