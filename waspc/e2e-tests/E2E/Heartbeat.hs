-- | Periodically reports the tests that are still running, so a long or stuck
-- run doesn't look the same as a run that hangs before running anything.
module E2E.Heartbeat
  ( heartbeatReporter,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (atomically, check, readTVar, readTVarIO)
import Control.Monad (unless, (>=>))
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)
import Test.Tasty.Ingredients (Ingredient (..))
import Test.Tasty.Runners (Status (..), testsNames)
import Text.Printf (printf)

-- | Meant to be composed with the console reporter (see
-- 'Test.Tasty.Ingredients.composeReporters').
heartbeatReporter :: Ingredient
heartbeatReporter = TestReporter [] $ \options testTree -> Just $ \statusMap -> do
  -- Tasty numbers the tests in the same order it lists them.
  let testNames = IntMap.fromList $ zip [0 ..] (testsNames options testTree)
  startTime <- getCurrentTime
  let waitForAllTestsToFinish = atomically $ mapM_ (readTVar >=> check . isDone) statusMap
      reportRunningTests = do
        now <- getCurrentTime
        statuses <- mapM readTVarIO statusMap
        let runningTests = [IntMap.findWithDefault "?" index testNames | (index, Executing _) <- IntMap.toList statuses]
        unless (null runningTests) $ do
          printf "[%dm] Still running: %s\n" (round (diffUTCTime now startTime / 60) :: Int) (intercalate ", " runningTests)
          hFlush stdout
      loop =
        race (threadDelay $ 60 * 1000000) waitForAllTestsToFinish >>= \case
          Left () -> reportRunningTests >> loop
          Right () -> return ()
  loop
  return $ \_ -> return True
  where
    isDone (Done _) = True
    isDone _ = False
