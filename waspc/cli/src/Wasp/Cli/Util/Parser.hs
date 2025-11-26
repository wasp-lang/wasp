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
  dispatch $ parseArguments cmdName parser args
  where
    dispatch (Success result) = do
      onSuccess result
    dispatch (Failure _ helpMessage) = do
      throwError $ CommandError "Parsing arguments failed" helpMessage
    dispatch (Stop helpMessage) = do
      cliSendMessageC $ Msg.Info helpMessage
      return ()

data ArgsResult a
  = Success a
  | Failure Int String
  | Stop String

parseArguments :: String -> Opt.Parser a -> Arguments -> ArgsResult a
parseArguments cmdName parser args =
  parserResultToArgsResult cmdName $
    Opt.execParserPure Opt.defaultPrefs parserInfo args
  where
    parserInfo = Opt.info (parser <**> Opt.helper) Opt.fullDesc

parserResultToArgsResult :: String -> Opt.ParserResult a -> ArgsResult a
parserResultToArgsResult _ (Opt.Success success) = Success success
parserResultToArgsResult cmdName (Opt.CompletionInvoked _) =
  error $ "Completion invoked when parsing '" <> cmdName <> "', but this should never happen"
parserResultToArgsResult cmdName (Opt.Failure failure) =
  failureToArgsResult $ Opt.execFailure failure cmdName
  where
    failureToArgsResult (help, EC.ExitSuccess, _) = Stop $ show help
    failureToArgsResult (help, EC.ExitFailure exitCodeInt, _) = Failure exitCodeInt $ show help
