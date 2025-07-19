module Wasp.Cli.Util.Parser
  ( parseArguments,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Command.Call (Arguments)

parseArguments :: String -> Opt.Parser a -> Arguments -> Either String a
parseArguments cmdName parser args =
  parserResultToEither cmdName $
    Opt.execParserPure Opt.defaultPrefs parserInfo args
  where
    parserInfo = Opt.info (parser Opt.<**> Opt.helper) Opt.fullDesc

parserResultToEither :: String -> Opt.ParserResult a -> Either String a
parserResultToEither _ (Opt.Success success) = Right success
parserResultToEither cmdName (Opt.CompletionInvoked _) =
  error $ "Completion invoked when parsing '" <> cmdName <> "', but this should never happen"
parserResultToEither cmdName (Opt.Failure failure) = Left $ show help
  where
    (help, _, _) = Opt.execFailure failure cmdName
