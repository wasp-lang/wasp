{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    checkRequirement,
    CommandError (..),
    CommandRequirement (..),
  )
where

import Control.Monad.Except (ExceptT, MonadError, MonadTrans, runExceptT, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (StateT (runStateT), gets, modify)
import Data.Foldable (find)
import System.Exit (exitFailure)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg

newtype Command a = Command {_runCommand :: RequiresT (ExceptT CommandError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError, MonadRequires)

runCommand :: Command a -> IO ()
runCommand cmd = do
  runExceptT (runStateT (unRequiresT $ _runCommand cmd) []) >>= \case
    Left cmdError -> do
      cliSendMessage $ Msg.Failure (_errorTitle cmdError) (_errorMsg cmdError)
      exitFailure
    Right _ -> return ()

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError {_errorTitle :: !String, _errorMsg :: !String}

-- | A requirement that a command needs in order to run
data CommandRequirement
  = -- | Requires that it is possible to connect to the prisma database.
    DbConnection
  deriving (Eq, Show)

-- | A monad transformer that holds a list of satisfied 'CommandRequirement's
-- that is updated whenever a new requirement is checked.
--
-- This is the canonical implementation of 'MonadRequires'.
newtype RequiresT m a = RequiresT {unRequiresT :: StateT [CommandRequirement] m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadError b)

-- | The Requires monad keeps track of which 'CommandRequirements' have been
-- checked and met.
--
-- NOTE: We go through the trouble of creating this abstraction so we can make
-- sure the set of satisfied requirements is never modified in a way we do not
-- want it to be (e.g. marking a requirement as met without actually checking
-- it).
class Monad m => MonadRequires m where
  -- | @checkRequirement requirement check@ runs @check@ if the requirement
  -- has not been met yet. @check@ should return whether the requirement is
  -- met.
  --
  -- @checkRequirement requirement check@ returns @True@ when the requirement
  -- is met, and will not run the check if the requirement has already been
  -- checked and met.
  checkRequirement :: CommandRequirement -> m Bool -> m Bool

instance Monad m => MonadRequires (RequiresT m) where
  checkRequirement r check =
    RequiresT $
      gets (find (== r)) >>= \case
        Nothing -> do
          -- If the requirement isn't in the list, check it and add it to the
          -- list only when the check returns True.
          met <- unRequiresT check
          when met $ modify (r :)
          return met
        Just _ -> return True
