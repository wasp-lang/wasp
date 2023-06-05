{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command.Requires
  ( -- * Command Requirements

    -- Command might have certain requirements that must be met before it runs.
    -- This module exports the lower-level parts of the command requirement
    -- API. To use these features to specify a requirement for a command,
    -- use 'Wasp.Cli.Command.Common.require'. For convenience,
    -- 'CommandRequirement' and its constructors are also exported from that
    -- module.
    --
    -- Call @require SomeRequirementConstructor@ anywhere in your command,
    -- preferably near the beginning of the command implementation and definitely
    -- before using the requirement.
    CommandRequirement (..),
    RequiresT,
    runRequiresT,
    MonadRequires (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify, when)
import Control.Monad.Trans (MonadTrans)
import Data.Foldable (find)

-- | A requirement that a command needs in order to run.
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

runRequiresT :: Monad m => RequiresT m a -> m a
runRequiresT m = evalStateT (unRequiresT m) []

-- | The Requires monad keeps track of which 'CommandRequirements' have been
-- checked and met.
--
-- NOTE: We go through the trouble of creating this abstraction so we can make
-- sure the set of satisfied requirements is never modified in a way we do not
-- want it to be (e.g. marking a requirement as met without actually checking
-- it).
class Monad m => MonadRequires m where
  -- | @checkRequirement requirement doCheck@ returns True if specified requirement
  -- is met, otherwise False. It will run `doCheck` to check this, unless it
  -- already knows from before that requirement is met.
  --
  -- This assumes that once a requirement is met, it will stay met for the
  -- rest of the program.
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
