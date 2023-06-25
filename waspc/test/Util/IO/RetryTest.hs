{-# LANGUAGE FlexibleInstances #-}

module Util.IO.RetryTest where

import Control.Monad.State (MonadState (get), State, modify, runState)
import Numeric.Natural (Natural)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.Util.IO.Retry as R

spec_RetryTest :: Spec
spec_RetryTest = do
  describe "retry" $ do
    describe "when action succeeds on the first try" $ do
      it "runs action only once" $ do
        runMockRetry (R.constPause 42) 2 (mkAction (NumFails 0))
          `shouldBe` (Right (), [ActionCall])
    describe "when action fails 2 times and then succeeds" $ do
      let action = mkAction (NumFails 2)
      describe "and maxNumRetries >= 2" $ do
        it "will run it 3 times and end with success" $ do
          let runTest = \maxNumRetries ->
                runMockRetry (R.constPause 42) maxNumRetries action
                  `shouldBe` (Right (), [ActionCall, ThreadDelayCall 42, ActionCall, ThreadDelayCall 42, ActionCall])
          mapM_ runTest [2, 3, 4, 10]
      describe "and maxNumRetries < 2" $ do
        it "will run it (maxNumRetries + 1) times and end with failure" $ do
          runMockRetry (R.constPause 42) 1 action
            `shouldBe` (Left (), [ActionCall, ThreadDelayCall 42, ActionCall])
          runMockRetry (R.constPause 42) 0 action
            `shouldBe` (Left (), [ActionCall])

-- TODO: Write more tests, refactor/simplify.

runMockRetry :: R.PauseStrategy -> Natural -> MockAction -> (Either () (), [Event])
runMockRetry pause maxNumRetries action = runState (R.retry pause maxNumRetries action) []

type MockAction = MockRetryMonad (Either () ())

mkAction :: NumFails -> MockAction
mkAction (NumFails numFails) = do
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
  rThreadDelay microseconds = do
    modify (++ [ThreadDelayCall microseconds])
