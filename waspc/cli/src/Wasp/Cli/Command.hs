{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    runCommandSilently,
    getConsole,
    CommandError (..),
    commandError,
    commandErrorWithToolOutput,
    compileCommandError,
    errorMessage,
    errorToolOutput,
    reportCommandError,
    reportCompileErrors,
    reportCompileWarnings,
    abortCommand,

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
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Data (Typeable, cast)
import Data.List (intercalate)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import System.Exit (ExitCode, exitFailure)
import Wasp.Cli.Console (Console (..), isPromptInputEOF)
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg
import Wasp.Problem (Problem (..), ToolOutput, problem, problemWithToolOutput)
import Wasp.Util (indent, trim)
import Wasp.Util.IO.Process (OutputStream (..))
import Wasp.Util.IO.Retry (MonadRetry (..))
import Wasp.Util.TextStyle (styleLiteral)

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
    Aborted stream -> do
      console.writeRawOutput stream "Aborted.\n"
      exitFailure
    HitBug unexpectedException -> do
      console.reporter.send $
        Msg.Failure
          "This is a bug in Wasp"
          ( "Please report it at "
              ++ styleLiteral newIssueUrl
              ++ " and include the following error:\n"
              ++ indent 2 (E.displayException unexpectedException)
          )
          Nothing
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
data CommandStop = AbortedByUser
  deriving (Show)

instance E.Exception CommandStop

-- | For when the user decides not to go on with the command.
abortCommand :: (MonadIO m) => m a
abortCommand = liftIO $ E.throwIO AbortedByUser

getConsole :: Command Console
getConsole = Command ask

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError
  { _headline :: !String,
    _errors :: ![Problem],
    _warnings :: ![Problem],
    -- | A failed compile is printed in its own way: every error and every
    -- warning is a message of its own, errors first. The headline is left
    -- out, because the line that ends the compile's task already says it.
    _isHeadlineLeftOut :: !Bool
  }

commandError :: String -> String -> CommandError
commandError headline message = commandErrorFromProblem headline $ problem message

-- | For an error that comes from a tool that Wasp ran.
commandErrorWithToolOutput :: String -> ToolOutput -> CommandError
commandErrorWithToolOutput headline failedToolOutput = commandErrorFromProblem headline $ problemWithToolOutput "" failedToolOutput

commandErrorFromProblem :: String -> Problem -> CommandError
commandErrorFromProblem headline commandProblem =
  CommandError
    { _headline = headline,
      _errors = [commandProblem],
      _warnings = [],
      _isHeadlineLeftOut = False
    }

compileCommandError :: String -> [Problem] -> [Problem] -> CommandError
compileCommandError headline errors warnings =
  CommandError
    { _headline = headline,
      _errors = errors,
      _warnings = warnings,
      _isHeadlineLeftOut = True
    }

errorMessage :: CommandError -> String
errorMessage = intercalate "\n" . map (.description) . _errors

errorToolOutput :: CommandError -> Maybe ToolOutput
errorToolOutput = listToMaybe . mapMaybe (.toolOutput) . _errors

reportCommandError :: Reporter -> CommandError -> IO ()
reportCommandError reporter cmdError
  | _isHeadlineLeftOut cmdError = do
      reportCompileErrors reporter $ _errors cmdError
      reportCompileWarnings reporter $ _warnings cmdError
  | otherwise = reporter.send $ Msg.Failure (_headline cmdError) (errorMessage cmdError) (errorToolOutput cmdError)

reportCompileErrors :: Reporter -> [Problem] -> IO ()
reportCompileErrors reporter = mapM_ $ reporter.send . makeMessageFromProblem Msg.Failure

reportCompileWarnings :: Reporter -> [Problem] -> IO ()
reportCompileWarnings reporter = mapM_ $ reporter.send . makeMessageFromProblem Msg.Warning

makeMessageFromProblem :: (String -> String -> Maybe ToolOutput -> Msg.Message) -> Problem -> Msg.Message
makeMessageFromProblem makeMessage compileProblem = makeMessage headline details compileProblem.toolOutput
  where
    (headline, details) = splitIntoHeadlineAndDetails compileProblem.description

-- | The compiler describes an error or a warning with a single text, whose
-- first line says what it is about.
splitIntoHeadlineAndDetails :: String -> (String, String)
splitIntoHeadlineAndDetails text = (headline, dropWhile (== '\n') details)
  where
    (headline, details) = break (== '\n') $ trim text

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
