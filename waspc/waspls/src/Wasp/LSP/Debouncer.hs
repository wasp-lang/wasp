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

-- | Debounce events named with type @k@. Each unique @k@ (by its 'Eq' instance)
-- has its own debounce timer. Construct a debouncer with 'newDebouncerIO'.
--
-- See 'debounce' for how to use it.
newtype Debouncer k = Debouncer (STM.Map k (Async ()))

newDebouncerIO :: IO (Debouncer k)
newDebouncerIO = Debouncer <$> STM.newIO

-- | @debounce debouncer waitMicros event action@ waits @waitMicros@ microseconds
-- and then runs @action@.
--
-- If 'debounce' is called again with the same @event@, only the newer call
-- fires.
debounce :: (MonadUnliftIO m, MonadIO m, Eq k, Hashable k) => Debouncer k -> Int -> k -> m () -> m ()
debounce (Debouncer running) waitMicros event fire = do
  fireIO <- toIO fire
  a <- liftIO $
    async $ do
      threadDelay waitMicros
      fireIO
      atomically $ STM.delete event running
  prev <- liftIO $
    atomically $ do
      prev <- STM.lookup event running
      STM.insert a event running
      return prev
  liftIO $ traverse_ cancel prev
