module Wasp.Cli.Util.Parser
  ( parseArguments,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Options.Applicative as Opt
import System.Exit (ExitCode (..))
import Wasp.Cli.Command.Call (Arguments)

parseArguments :: (MonadIO m) => String -> Opt.Parser a -> Arguments -> m (Either String a)
parseArguments cmdName parser args = liftIO $ do
  let parserInfo = Opt.info (parser Opt.<**> Opt.helper) Opt.fullDesc
  let result = Opt.execParserPure Opt.defaultPrefs parserInfo args
  parserResultToEither cmdName result

parserResultToEither :: String -> Opt.ParserResult a -> IO (Either String a)
parserResultToEither _ (Opt.Success success) = return $ Right success
parserResultToEither cmdName (Opt.CompletionInvoked _) =
  error $ "Completion invoked when parsing '" <> cmdName <> "', but this should never happen"
parserResultToEither cmdName (Opt.Failure failure) = do
  let (help, exitCode, _) = Opt.execFailure failure cmdName
  case exitCode of
    ExitSuccess -> do
      -- This is a help request, not an error. Use handleParseResult to print help and exit with code 0.
      Opt.handleParseResult (Opt.Failure failure)
    ExitFailure _ ->
      -- This is an actual parsing error
      return $ Left $ show help
