module Wasp.LSP.DebouncerTest
  ( spec_Debouncer,
  )
where

import Control.Concurrent (newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (void)
import Data.Maybe (isJust)
import System.Timeout (timeout)
import Test.Tasty.Hspec
import Wasp.LSP.Debouncer (debounce, newDebouncerIO)

-- | Debounce time for all tests in microseconds.
debounceTime :: Int
debounceTime = 1000

-- | How long to wait until we decide a test is never going to finish.
timeoutTime :: Int
timeoutTime = 1 * 1000 * 1000

spec_Debouncer :: Spec
spec_Debouncer = describe "Wasp.LSP.Debouncer" $ do
  -- To test this multithreaded code, which can have nondeterminism problems due
  -- to randomness in what order the threads get woken up, these tests follow
  -- this general pattern:
  --
  -- 1. Create an empty MVar to track when actions get run.
  -- 2. Debounce actions to fill those MVars.
  -- 3. Wait on those MVars to be filled (with a timeout) and check their
  --    properties.
  -- 4. If we want to make sure the action was run, check to make sure the
  --    timeout wasn't reached, which is done by checking if the return value is
  --    'Just'.
  --
  -- TODO: find a robust  way to test that actions get debounced when run close
  -- enough to each other in time.

  it "eventually runs the action" $ do
    debouncer <- newDebouncerIO
    mvar <- newEmptyMVar

    -- Debounce filling the mvar and wait for it to be filled.
    debounce debouncer debounceTime () (void $ tryPutMVar mvar ())
    timedOut <- timeout timeoutTime $ readMVar mvar >>= (`shouldBe` ())
    timedOut `shouldSatisfy` isJust

  it "doesn't debounce actions for different events" $ do
    debouncer <- newDebouncerIO
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    -- Run two actions with different events.
    debounce debouncer debounceTime 'a' (void $ tryPutMVar mvar1 ())
    debounce debouncer debounceTime 'b' (void $ tryPutMVar mvar2 ())

    -- Check that both actions executed.
    timedOut <- timeout timeoutTime $ do
      readMVar mvar1 >>= (`shouldBe` ())
      readMVar mvar2 >>= (`shouldBe` ())
    timedOut `shouldSatisfy` isJust

  it "can run multiple actions with the same event" $ do
    debouncer <- newDebouncerIO
    mvar <- newEmptyMVar

    -- Run an action, wait for it to run, and check that it ran.
    debounce debouncer debounceTime () (void $ tryPutMVar mvar ())
    firstTimedOut <- timeout timeoutTime $ readMVar mvar
    firstTimedOut `shouldSatisfy` isJust

    -- Run the second action, wait for it to run, and check that it ran.
    debounce debouncer debounceTime () (void $ tryPutMVar mvar ())
    secondTimedOut <- timeout timeoutTime $ readMVar mvar
    secondTimedOut `shouldSatisfy` isJust
