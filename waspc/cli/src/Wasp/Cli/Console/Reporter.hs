{-# LANGUAGE RankNTypes #-}

module Wasp.Cli.Console.Reporter
  ( Reporter (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Wasp.Message as Msg

-- | How Wasp says things to the user.
data Reporter = Reporter
  { send :: Msg.Message -> IO (),
    -- | Runs a step of work and tells the user how it goes. It takes the text
    -- to show while the step runs and the text to show once it succeeds. A
    -- step that fails says nothing of its own, and its error passes on
    -- unchanged.
    --
    -- Record dot syntax doesn't work on this field, so call it as
    -- @runTaskWithStatusReport reporter@.
    runTaskWithStatusReport :: forall m a. (MonadIO m) => String -> String -> m a -> m a
  }
