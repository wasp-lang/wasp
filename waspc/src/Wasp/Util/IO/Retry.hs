{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Util.IO.Retry
  ( retry,
    constPause,
    linearPause,
    expPause,
    customPause,
    MonadRetry (..),
    PauseStrategy,
  )
where

import Control.Concurrent (threadDelay)
import Numeric.Natural (Natural)
import Prelude hiding (readFile, writeFile)

-- | Runs given action, and then if it fails, retries it, up to maxNumRetries.
--   Uses provided pauseStrategy to calculate pause between tries.
retry :: forall m e a. (MonadRetry m) => PauseStrategy -> Natural -> m (Either e a) -> m (Either e a)
retry (PauseStrategy calcPause) maxNumRetries action = go 0
  where
    maxNumTries :: Natural
    maxNumTries = maxNumRetries + 1

    go :: Natural -> m (Either e a)
    go numFailedTries =
      action >>= \case
        Right result -> pure $ Right result
        Left e ->
          let numFailedTries' = numFailedTries + 1
           in if numFailedTries' < maxNumTries
                then do
                  rThreadDelay $ fromIntegral $ calcPause numFailedTries'
                  go numFailedTries'
                else pure $ Left e

class (Monad m) => MonadRetry m where
  rThreadDelay :: Int -> m ()

instance MonadRetry IO where
  rThreadDelay = threadDelay

newtype PauseStrategy = PauseStrategy (NumFailedTries -> Microseconds)

type Microseconds = Natural

type NumFailedTries = Natural

constPause :: Microseconds -> PauseStrategy
constPause basePause = PauseStrategy (const basePause)

linearPause :: Microseconds -> PauseStrategy
linearPause basePause = PauseStrategy (* basePause)

expPause :: Microseconds -> PauseStrategy
expPause basePause = PauseStrategy $ \i -> basePause * 2 ^ (i - 1)

customPause :: (NumFailedTries -> Microseconds) -> PauseStrategy
customPause = PauseStrategy
