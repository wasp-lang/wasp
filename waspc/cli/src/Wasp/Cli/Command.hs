{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    CommandError (..),

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
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Data (Typeable, cast)
import Data.Maybe (mapMaybe)
import System.Exit (exitFailure)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg
import Wasp.Util.IO.Retry (MonadRetry (..))

newtype Command a = Command {_runCommand :: StateT [Requirement] (ExceptT CommandError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError, MonadThrow, MonadCatch, MonadMask)

instance MonadRetry Command where
  rThreadDelay = liftIO . threadDelay

runCommand :: Command a -> IO ()
runCommand cmd =
  E.try (runExceptT (flip evalStateT [] $ _runCommand cmd)) >>= \case
    Left (Interactive.StdinNotInteractive usage) -> do
      cliSendMessage
        $ Msg.Failure "Interactive terminal required"
        $ "Wasp needs to ask you something, but stdin is not an interactive terminal.\n"
          <> "Either run this command from a terminal, or pass everything it needs as arguments:\n\n"
          <> usage
      exitFailure
    Left Interactive.PromptCancelled -> do
      putStrLn "Aborted."
      exitFailure
    Right (Left cmdError) -> do
      cliSendMessage $ Msg.Failure (_errorTitle cmdError) (_errorMsg cmdError)
      exitFailure
    Right (Right _) -> return ()

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError {_errorTitle :: !String, _errorMsg :: !String}

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
