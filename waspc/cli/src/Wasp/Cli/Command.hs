{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    runCommandSilently,
    getConsole,
    CommandError (..),
    commandError,
    compileCommandError,
    errorMessage,
    reportCommandError,
    reportCompileErrors,
    reportCompileWarnings,
    compilationWarningsTitle,
    formatErrorOrWarningMessages,
    abortCommand,
    failCommandWithoutError,

    -- * Requirements

    -- There are some requirements we want to assert in command code, such as
    -- ensuring the command is being run inside a wasp project directory. We
    -- might end up wanting to check each requirement multiple times, especially
    -- if we want the value from it (like getting the wasp project directory),
    -- but we also want to avoid duplicating work. Using 'require' results in
    -- checked requirements being stored so they can be immediately retrieved
    -- when checking the same requirements additional times.
    --
    -- See instances of 'Requirable' (each in its own module under
    -- @Wasp.Cli.Command.Require@) for what kinds of requirements are supported.
    -- To implement a new requirable type, give your type an instance of
    -- 'Requirable' in its own module under @Wasp.Cli.Command.Require@.
    require,
    Requirable (checkRequirement),
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Data (Typeable, cast)
import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import System.Exit (ExitCode, exitFailure)
import Wasp.Cli.Console (Console (..), isPromptInputEOF)
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg
import Wasp.Util (indent)
import Wasp.Util.IO.Process (OutputStream (..))
import Wasp.Util.IO.Retry (MonadRetry (..))

newtype Command a = Command {_runCommand :: ReaderT Console (StateT [Requirement] (ExceptT CommandError IO)) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError, MonadThrow, MonadCatch, MonadMask)

instance MonadRetry Command where
  rThreadDelay = liftIO . threadDelay

-- | Runs the command and ends the program if it didn't succeed. This is the
-- only place that prints a command's error and picks the exit code.
runCommand :: Console -> Command a -> IO ()
runCommand console cmd =
  tryRunningCommand console cmd >>= \case
    Succeeded _ -> return ()
    Failed cmdError -> do
      reportCommandError console.reporter cmdError
      exitFailure
    FailedWithoutError -> exitFailure
    Aborted stream -> do
      console.writeRawOutput stream "Aborted.\n"
      exitFailure
    HitBug unexpectedException -> do
      console.writeRawOutput Stdout . T.pack $
        "\nThis is a bug in Wasp. Please report it at "
          ++ newIssueUrl
          ++ " and include the following error:\n"
          ++ indent 2 (E.displayException unexpectedException)
          ++ "\n"
      exitFailure
  where
    newIssueUrl = "https://github.com/wasp-lang/wasp/issues/new"

-- | Runs the command for its effects only: it never prints an error and never
-- ends the program, whatever happens in the command.
runCommandSilently :: Console -> Command a -> IO ()
runCommandSilently console cmd = void $ tryRunningCommand console cmd

data CommandOutcome a
  = Succeeded a
  | Failed CommandError
  | FailedWithoutError
  | -- | The user stopped the command. Holds the stream that gets told about it.
    Aborted OutputStream
  | -- | The command threw an exception that nothing in Wasp expects.
    HitBug E.SomeException

tryRunningCommand :: Console -> Command a -> IO (CommandOutcome a)
tryRunningCommand console cmd =
  (either Failed Succeeded <$> runExceptT (flip evalStateT [] $ flip runReaderT console $ _runCommand cmd))
    `E.catch` \(exception :: E.SomeException) ->
      maybe (E.throwIO exception) return (getOutcomeOfException exception)

-- | Returns 'Nothing' for the exceptions that are not a command's outcome and
-- have to pass through: the ones that end the program with an exit code, and
-- the ones that stop a thread from the outside, like Ctrl+C.
getOutcomeOfException :: E.SomeException -> Maybe (CommandOutcome a)
getOutcomeOfException exception
  | Just AbortedByUser <- E.fromException exception = Just $ Aborted Stderr
  | Just StoppedWithoutError <- E.fromException exception = Just FailedWithoutError
  | hasUserEndedPromptInput = Just $ Aborted Stdout
  | isExit || isAsyncException = Nothing
  | otherwise = Just $ HitBug exception
  where
    -- The user ended the input (Ctrl+D) while a prompt was waiting for an answer.
    hasUserEndedPromptInput = maybe False isPromptInputEOF (E.fromException exception)
    isExit = isJust (E.fromException exception :: Maybe ExitCode)
    isAsyncException = isJust (E.fromException exception :: Maybe E.SomeAsyncException)

-- | Stops a command from anywhere in its code, without it being an error that
-- 'runCommand' prints.
data CommandStop = AbortedByUser | StoppedWithoutError
  deriving (Show)

instance E.Exception CommandStop

-- | For when the user decides not to go on with the command.
abortCommand :: (MonadIO m) => m a
abortCommand = liftIO $ E.throwIO AbortedByUser

-- | For a command that fails after it has already said why.
failCommandWithoutError :: (MonadIO m) => m a
failCommandWithoutError = liftIO $ E.throwIO StoppedWithoutError

getConsole :: Command Console
getConsole = Command ask

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError
  { _headline :: !String,
    _errors :: ![String],
    _warnings :: ![String],
    -- | A failed compile is printed in its own way: the errors and the
    -- warnings come first, each under their own title, and the headline comes
    -- last, with this text under it. 'Nothing' for all the other errors.
    _compileSummary :: !(Maybe String)
  }

commandError :: String -> String -> CommandError
commandError headline message =
  CommandError
    { _headline = headline,
      _errors = [message],
      _warnings = [],
      _compileSummary = Nothing
    }

compileCommandError :: String -> String -> [String] -> [String] -> CommandError
compileCommandError headline summary errors warnings =
  CommandError
    { _headline = headline,
      _errors = errors,
      _warnings = warnings,
      _compileSummary = Just summary
    }

errorMessage :: CommandError -> String
errorMessage = intercalate "\n" . _errors

reportCommandError :: Reporter -> CommandError -> IO ()
reportCommandError reporter cmdError = case _compileSummary cmdError of
  Nothing -> reporter.send $ Msg.Failure (_headline cmdError) (errorMessage cmdError)
  Just compileSummary -> do
    reportCompileErrors reporter $ _errors cmdError
    reportCompileWarnings reporter $ _warnings cmdError
    reporter.send $ Msg.Failure (_headline cmdError) compileSummary

reportCompileErrors :: Reporter -> [String] -> IO ()
reportCompileErrors reporter errors =
  unless (null errors)
    $ reporter.send
    $ Msg.Failure "Your wasp project failed to compile"
    $ formatErrorOrWarningMessages errors

reportCompileWarnings :: Reporter -> [String] -> IO ()
reportCompileWarnings reporter warnings =
  unless (null warnings)
    $ reporter.send
    $ Msg.Warning compilationWarningsTitle
    $ formatErrorOrWarningMessages warnings

compilationWarningsTitle :: String
compilationWarningsTitle = "Your wasp project reported following warnings during compilation"

formatErrorOrWarningMessages :: [String] -> String
formatErrorOrWarningMessages = intercalate "\n" . map ("- " ++)

data Requirement where
  Requirement :: (Requirable r) => r -> Requirement

class (Typeable r) => Requirable r where
  -- | Check if the requirement is met and return a value representing that
  -- requirement.
  --
  -- This function must always return a value: if the requirement is not met,
  -- throw a 'CommandError'.
  checkRequirement :: Command r

-- | Assert that a requirement is met and receive information about that
-- requirement, if any is offered.
--
-- To use, pattern match on the result, e.g.
--
-- @
-- do
--   HasDbConnection <- require
-- @
require :: (Requirable r) => Command r
require =
  Command (gets (mapMaybe cast)) >>= \case
    (req : _) -> return req
    [] -> do
      -- Requirement hasn't been met, so run the check
      req <- checkRequirement
      Command $ modify (Requirement req :)
      return req
