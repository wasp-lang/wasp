module Wasp.LSP.DebouncerTest
  ( spec_Debouncer,
  )
where

import Control.Concurrent (newEmptyMVar, threadDelay, tryPutMVar, tryReadMVar)
import Control.Monad (replicateM_, void)
import GHC.Conc (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Test.Tasty.Hspec
import Wasp.LSP.Debouncer (debounce, newDebouncerIO)

spec_Debouncer :: Spec
spec_Debouncer = describe "Wasp.LSP.Debouncer" $ do
  it "runs the action" $ do
    debouncer <- newDebouncerIO
    mvar <- newEmptyMVar

    debounce debouncer 1000 () (void $ tryPutMVar mvar ())
    threadDelay 20000

    tryReadMVar mvar >>= (`shouldBe` Just ())

  it "doesn't debounce actions for different events" $ do
    debouncer <- newDebouncerIO
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    debounce debouncer 1000 'a' (void $ tryPutMVar mvar1 ())
    debounce debouncer 1000 'b' (void $ tryPutMVar mvar2 ())
    threadDelay 20000

    tryReadMVar mvar1 >>= (`shouldBe` Just ())
    tryReadMVar mvar2 >>= (`shouldBe` Just ())

  it "debounces actions with the same event" $ do
    debouncer <- newDebouncerIO
    countTVar <- newTVarIO (0 :: Int)

    replicateM_ 2 $
      debounce debouncer 1000 () (atomically $ readTVar countTVar >>= (writeTVar countTVar . (+ 1)))
    threadDelay 20000

    readTVarIO countTVar >>= (`shouldBe` 1)

  it "executes multiple actions from the same event given enough time" $ do
    debouncer <- newDebouncerIO
    countTVar <- newTVarIO (0 :: Int)

    debounce debouncer 1000 () (atomically $ readTVar countTVar >>= (writeTVar countTVar . (+ 1)))
    threadDelay 20000
    debounce debouncer 1000 () (atomically $ readTVar countTVar >>= (writeTVar countTVar . (+ 1)))
    threadDelay 20000

    readTVarIO countTVar >>= (`shouldBe` 2)
