module Wasp.LSP.Debouncer
  ( Debouncer,
    newDebouncerIO,
    debounce,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO, toIO)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import qualified StmContainers.Map as STM

-- | @debounce debouncer waitMicros event fire@ prevents certain actions from
-- running too often by ignoring repeated attempts to run the action within a
-- certain period of time. This is done by \"debouncing\" actions with the same
-- @event@, which is a label for what is triggering this action to run.
--
-- When 'debounce' is called, it waits @waitMicros@ microseconds before running
-- the action @fire@. If 'debounce' is called again within @waitMicros@
-- microseconds, the previous action is cancelled and only @fire@ from the newer
-- call runs. The new call waits another @waitMicros@ and can also be cancelled.
--
-- Use this function to reduce the number of times some action is run. For example,
-- refreshing the list of exported symbols from TS files on file change is debounced,
-- since we don't want to do that after every key press in the editor.
--
-- ==== __Example__
-- @
-- do
--   debouncer <- newDebouncerIO
--   debounce debouncer 1000 "event a" $ putStrLn "Event A #1"
--   threadDelay 100
--   debounce debouncer 1000 "event a" $ putStrLn "Event A #2"
--   threadDelay 2000
-- @
-- Prints just @Event A #2@.
--
-- ==== __Example__
-- @
-- do
--   debouncer <- newDebouncerIO
--   debounce debouncer 1000 "event a" $ putStrLn "Event A #1"
--   threadDelay 100
--   debounce debouncer 1000 "event b" $ putStrLn "Event B #1"
--   threadDelay 2000
-- @
-- Prints both @Event A #1@ and @Event B #1@.
debounce :: (MonadUnliftIO m, MonadIO m, Eq k, Hashable k) => Debouncer k -> Int -> k -> m () -> m ()
debounce (Debouncer running) waitMicros event fire = do
  fireIO <- toIO fire

  -- Spawn a new thread that waits @waitMicros@ before running @fireIO@.
  newDelayedAction <- liftIO $
    async $ do
      threadDelay waitMicros
      fireIO
      -- Mark this event as inactive by removing it from the running events.
      atomically $ STM.delete event running

  -- Atomically replace the previous action for this event (if any) with
  -- @newDelayedAction@.
  prevDelayedAction <- liftIO $
    atomically $ do
      prevAction <- STM.lookup event running
      STM.insert newDelayedAction event running
      return prevAction

  -- Cancel the previous action for this event (if any).
  liftIO $ traverse_ cancel prevDelayedAction

-- | Debounce events named with type @k@. Each unique @k@ (by its 'Eq' instance)
-- has its own debounce timer. Construct a debouncer with 'newDebouncerIO'.
--
-- See 'debounce' for how to use it.
newtype Debouncer k = Debouncer
  { -- | A thread-safe map of event labels to async actions. This stores
    -- actions for each event that are waiting for their debounce timers to end.
    -- This is needed so that new calls to 'debounce' can cancel the currently
    -- running actions before they can finish running.
    debouncerRunningEvents :: STM.Map k (Async ())
  }

newDebouncerIO :: IO (Debouncer k)
newDebouncerIO = Debouncer <$> STM.newIO
