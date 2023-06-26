{-# LANGUAGE FlexibleInstances #-}

module Util.IO.RetryTest where

import Control.Monad (forM_)
import Control.Monad.State (MonadState (get), State, modify, runState)
import Numeric.Natural (Natural)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.Util.IO.Retry as R

spec_RetryTest :: Spec
spec_RetryTest = do
  describe "retry" $ do
    describe "when action succeeds on the first try" $ do
      it "runs action only once" $ do
        runMockRetry (R.constPause 42) 2 (mockAction (NumFails 0))
          `shouldBe` (Right (), [ActionCall])
    describe "when action fails 2 times and then succeeds" $ do
      let action = mockAction (NumFails 2)
      describe "and maxNumRetries >= 2" $ do
        it "will run it 3 times and end with success" $ do
          forM_ [2, 3, 4, 10] $ \maxNumRetries ->
            runMockRetry (R.constPause 42) maxNumRetries action
              `shouldBe` (Right (), [ActionCall, ThreadDelayCall 42, ActionCall, ThreadDelayCall 42, ActionCall])
      describe "and maxNumRetries < 2" $ do
        it "will run it (maxNumRetries + 1) times and end with failure" $ do
          runMockRetry (R.constPause 42) 0 action
            `shouldBe` (Left (), [ActionCall])
          runMockRetry (R.constPause 42) 1 action
            `shouldBe` (Left (), [ActionCall, ThreadDelayCall 42, ActionCall])
    describe "determines pauses according to provided pause strategy" $ do
      let action = mockAction (NumFails 3)
      let testPause = \pauseStrategy _expectedPauses@(p1, p2, p3) ->
            snd (runMockRetry pauseStrategy 5 action)
              `shouldBe` [ActionCall, ThreadDelayCall p1, ActionCall, ThreadDelayCall p2, ActionCall, ThreadDelayCall p3, ActionCall]
      it "for constPause" $ testPause (R.constPause 10) (10, 10, 10)
      it "for linearPause" $ testPause (R.linearPause 10) (10, 20, 30)
      it "for expPause" $ testPause (R.expPause 10) (10, 20, 40)
      it "for customPause" $ testPause (R.customPause (^ (2 :: Int))) (1, 4, 9)

runMockRetry :: R.PauseStrategy -> Natural -> MockAction -> (Either () (), [Event])
runMockRetry pause maxNumRetries action = runState (R.retry pause maxNumRetries action) []

type MockAction = MockRetryMonad (Either () ())

mockAction :: NumFails -> MockAction
mockAction (NumFails numFails) = do
  events <- get
  let result =
        if length (filter (== ActionCall) events) >= numFails
          then Right ()
          else Left ()
  modify (++ [ActionCall])
  return result

newtype NumFails = NumFails Int

data Event = ThreadDelayCall Int | ActionCall
  deriving (Show, Eq)

type MockRetryMonad = State [Event]

instance R.MonadRetry MockRetryMonad where
  rThreadDelay microseconds = modify (++ [ThreadDelayCall microseconds])
