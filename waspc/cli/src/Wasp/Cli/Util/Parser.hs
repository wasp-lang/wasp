module Wasp.Cli.Util.Parser
  ( withArguments,
    getParserHelpMessage,
  )
where

import Control.Applicative ((<**>))
import Control.Monad.Except (throwError)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt.Help
import Options.Applicative.Help.Core (parserHelp)
import qualified System.Exit as EC
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

withArguments :: String -> Opt.Parser a -> (a -> Command ()) -> Arguments -> Command ()
withArguments cmdName parser onSuccess args =
  case parseArguments cmdName parser args of
    (ArgsParsed result) -> onSuccess result
    (ParseFailure helpMessage) -> throwError $ CommandError "Parsing arguments failed" helpMessage
    (ShowHelp helpMessage) -> cliSendMessageC $ Msg.Info helpMessage

getParserHelpMessage :: Opt.Parser a -> String
getParserHelpMessage =
  Opt.Help.renderHelp (Opt.prefColumns parserPreferences) . parserHelp parserPreferences

data ArgsParseResult args
  = ArgsParsed args
  | ParseFailure String
  | ShowHelp String

parseArguments :: String -> Opt.Parser a -> Arguments -> ArgsParseResult a
parseArguments cmdName parser args =
  case Opt.execParserPure parserPreferences parserInfo args of
    (Opt.Success success) -> ArgsParsed success
    (Opt.CompletionInvoked _) ->
      error $ "Completion invoked when parsing '" <> cmdName <> "', but this should never happen"
    (Opt.Failure failure) ->
      case Opt.execFailure failure cmdName of
        (help, EC.ExitSuccess, _) -> ShowHelp $ show help
        (help, EC.ExitFailure _, _) -> ParseFailure $ show help
  where
    parserInfo = Opt.info (parser <**> Opt.helper) Opt.fullDesc

parserPreferences :: Opt.ParserPrefs
parserPreferences = Opt.defaultPrefs
