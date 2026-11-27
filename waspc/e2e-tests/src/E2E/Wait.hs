-- | Steps that wait until something holds, and how long they wait.
module E2E.Wait
  ( waitUntil,
    pollUntil,
    settleTimeoutSeconds,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import E2E.TestM (TestM, failTest)

-- | Checks the condition right away, and then every 'pollIntervalMicroseconds'
-- until it holds. Fails the test if it still doesn't hold after the given
-- number of seconds.
--
-- The deadline is only checked between checks, so a condition that runs a
-- command should give it a timeout no longer than the wait.
waitUntil :: String -> Int -> TestM Bool -> TestM ()
waitUntil description seconds condition = do
  holds <- pollUntil seconds condition
  unless holds $ failTest $ "Timed out after " ++ show seconds ++ "s waiting until " ++ description ++ "."

-- | Like 'waitUntil', but returns whether the condition held in time instead
-- of failing, for waits that fail with more context.
pollUntil :: Int -> TestM Bool -> TestM Bool
pollUntil seconds condition = do
  deadline <- liftIO $ addUTCTime (fromIntegral seconds) <$> getCurrentTime
  let poll = do
        holds <- condition
        now <- liftIO getCurrentTime
        if holds || now > deadline
          then return holds
          else liftIO (threadDelay pollIntervalMicroseconds) >> poll
  poll

-- | Short enough not to slow tests down, long enough not to keep a core busy.
pollIntervalMicroseconds :: Int
pollIntervalMicroseconds = 500000

-- | For a wait on something that follows an event we already waited for, e.g.
-- a process's children stopping after it exited.
settleTimeoutSeconds :: Int
settleTimeoutSeconds = 30
