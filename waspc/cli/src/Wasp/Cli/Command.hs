{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    CommandError (..),
    makeCommandError,

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
import Control.Monad (guard)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Data (Typeable, cast)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import System.Exit (exitFailure)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Diagnostic (Diagnostic)
import qualified Wasp.Diagnostic as Diagnostic
import Wasp.Diagnostic.ErrorId (ErrorId)
import qualified Wasp.Message as Msg
import Wasp.Util.IO.Retry (MonadRetry (..))
import qualified Wasp.Util.Interactive as Interactive

newtype Command a = Command {_runCommand :: StateT [Requirement] (ExceptT CommandError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError, MonadThrow, MonadCatch, MonadMask)

instance MonadRetry Command where
  rThreadDelay = liftIO . threadDelay

runCommand :: Command a -> IO ()
runCommand cmd =
  E.tryJust (guard . Interactive.isPromptInputEOF) (runExceptT (flip evalStateT [] $ _runCommand cmd)) >>= \case
    Left () -> do
      -- The user ended the input (Ctrl+D) while a prompt was waiting for an answer.
      cliSendMessage $ Msg.Info "\nAborted."
      exitFailure
    Right (Left (CommandError diagnostic)) -> do
      cliSendMessage $ Msg.Diagnostic diagnostic
      exitFailure
    Right (Right _) -> return ()

newtype CommandError = CommandError Diagnostic

makeCommandError :: ErrorId -> String -> String -> CommandError
makeCommandError errorId errorTitle errorDetails =
  CommandError $
    Diagnostic.makeError errorId errorTitle & Diagnostic.setDetails errorDetails

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
