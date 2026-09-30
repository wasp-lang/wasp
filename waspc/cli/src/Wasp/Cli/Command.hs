{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    ShutdownContext,
    checkForShutdown,
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

import Control.Concurrent (MVar, readMVar, tryReadMVar)
import Control.Concurrent.Async (race)
import Control.Exception (MaskingState (Unmasked), getMaskingState, throwIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Data (Typeable, cast)
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode, exitFailure)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg

newtype Command a = Command {_runCommand :: ReaderT ShutdownContext (StateT [Requirement] (ExceptT CommandError IO)) a}
  deriving (Functor, Applicative, Monad, MonadError CommandError, MonadThrow, MonadCatch, MonadMask)

runCommand :: ShutdownContext -> Command a -> IO ()
runCommand shutdown cmd = do
  runExceptT (flip evalStateT [] $ runReaderT (_runCommand $ cmd <* checkForShutdown) shutdown) >>= \case
    Left cmdError -> do
      cliSendMessage $ Msg.Failure (_errorTitle cmdError) (_errorMsg cmdError)
      exitFailure
    Right _ -> return ()

type ShutdownContext = MVar ExitCode

-- Normal Command IO is cancellable. Resource acquisition and release scoped
-- with Command-level bracket run to completion. A bracket inside one lifted IO
-- action remains inside that action's cancellation boundary.
instance MonadIO Command where
  liftIO action = Command $ do
    shutdown <- ask
    liftIO $ do
      masking <- getMaskingState
      if masking == Unmasked
        then do
          pending <- tryReadMVar shutdown
          case pending of
            Just exitCode -> throwIO exitCode
            Nothing -> race (readMVar shutdown) action >>= either throwIO return
        else action

checkForShutdown :: Command ()
checkForShutdown = Command $ do
  shutdown <- ask
  pending <- liftIO $ tryReadMVar shutdown
  maybe (return ()) (liftIO . throwIO) pending

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
