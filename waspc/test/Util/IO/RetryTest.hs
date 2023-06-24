{-# LANGUAGE FlexibleInstances #-}

module Util.IO.RetryTest where

import Control.Monad.State (State, modify, runState)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.Util.IO.Retry as R

spec_RetryTest :: Spec
spec_RetryTest = do
  describe "retry" $ do
    it "runs action only once if it succeeds on the first try" $ do
      let action = modify (++ [ActionCall]) >> return (Right () :: Either () ())
      let (result, events) = runState (R.retry (R.constPause 42) 2 action) []
      (result, events) `shouldBe` (Right (), [ActionCall])
    it "runs always failing action until it spends all retries" $ do
      let action = modify (++ [ActionCall]) >> return (Left () :: Either () ())
      let (result, events) = runState (R.retry (R.constPause 42) 2 action) []
      (result, events) `shouldBe` (Left (), [ActionCall, ThreadDelayCall 42, ActionCall, ThreadDelayCall 42, ActionCall])

-- TODO: Write more tests, refactor/simplify.

data Event = ThreadDelayCall Int | ActionCall
  deriving (Show, Eq)

instance R.MonadRetry (State [Event]) where
  rThreadDelay microseconds = do
    modify (++ [ThreadDelayCall microseconds])
