module Wasp.Cli.Util.Parser
  ( withArguments,
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

withArguments :: String -> Opt.Parser a -> (a -> Command ()) -> Arguments -> Command ()
withArguments cmdName parser onSuccess args =
  case parseArguments cmdName parser args of
    (ArgsParsed result) -> do
      onSuccess result
    (ParseFailure helpMessage) -> do
      throwError $ CommandError "Parsing arguments failed" helpMessage
    (ShowHelp helpMessage) -> do
      cliSendMessageC $ Msg.Info helpMessage
      return ()

data ArgsParseResult args
  = ArgsParsed args
  | ParseFailure String
  | ShowHelp String

parseArguments :: String -> Opt.Parser a -> Arguments -> ArgsParseResult a
parseArguments cmdName parser args =
  parserResultToArgsResult cmdName $
    Opt.execParserPure Opt.defaultPrefs parserInfo args
  where
    parserInfo = Opt.info (parser <**> Opt.helper) Opt.fullDesc

parserResultToArgsResult :: String -> Opt.ParserResult a -> ArgsParseResult a
parserResultToArgsResult _ (Opt.Success success) = ArgsParsed success
parserResultToArgsResult cmdName (Opt.CompletionInvoked _) =
  error $ "Completion invoked when parsing '" <> cmdName <> "', but this should never happen"
parserResultToArgsResult cmdName (Opt.Failure failure) =
  case Opt.execFailure failure cmdName of
    (help, EC.ExitSuccess, _) ->
      ShowHelp $ show help
    (help, EC.ExitFailure _, _) ->
      ParseFailure $ show help
