{-# LANGUAGE RankNTypes #-}

module Wasp.Cli.Console.Reporter
  ( Reporter (..),
    TaskEnd (..),
    runTaskWithFixedTexts,
  )
where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import qualified Wasp.Message as Msg

-- | How Wasp says things to the user.
data Reporter = Reporter
  { send :: Msg.Message -> IO (),
    -- | Runs a step of work and tells the user how it goes. It takes the text
    -- to show while the step runs, a function that says how the step ended
    -- from what it returned, and the text to show if the step throws (in @m@
    -- or as an exception). The error of a step that throws passes on
    -- unchanged.
    --
    -- Record dot syntax doesn't work on this field, so call it as
    -- @runTaskWithStatusReport reporter@.
    runTaskWithStatusReport :: forall m a. (MonadIO m, MonadMask m) => String -> (a -> TaskEnd) -> String -> m a -> m a
  }

-- | How a task ended, with the text to show for it.
data TaskEnd = TaskSucceeded String | TaskFailed String

-- | Runs a task that succeeds whenever it doesn't throw, so the text for its
-- success is known upfront.
runTaskWithFixedTexts :: (MonadIO m, MonadMask m) => Reporter -> String -> String -> String -> m a -> m a
runTaskWithFixedTexts reporter runningText successText =
  runTaskWithStatusReport reporter runningText (const $ TaskSucceeded successText)
