{-# LANGUAGE RankNTypes #-}

module Wasp.TaskRunner
  ( TaskRunner (..),
  )
where

import Control.Monad.IO.Class (MonadIO)

-- | Runs a step of work and tells the user how it goes. It takes the text to
-- show while the step runs and the text to show once it succeeds. A step that
-- fails says nothing of its own, and its error passes on unchanged.
--
-- It gives the compiler a way to report its progress without being able to
-- say anything else.
newtype TaskRunner = TaskRunner
  { runTask :: forall m a. (MonadIO m) => String -> String -> m a -> m a
  }
