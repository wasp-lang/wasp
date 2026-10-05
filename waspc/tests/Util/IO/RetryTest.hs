{-# LANGUAGE FlexibleInstances #-}

module Util.IO.RetryTest where

import Control.Monad (forM_)
import Control.Monad.State (MonadState (get), State, modify, runState)
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it, shouldBe)
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
            `shouldBe` (Left 1, [ActionCall])
          runMockRetry (R.constPause 42) 1 action
            `shouldBe` (Left 2, [ActionCall, ThreadDelayCall 42, ActionCall])
    describe "determines pauses according to provided pause strategy" $ do
      let action = mockAction (NumFails 3)
      let testPause = \pauseStrategy _expectedPauses@(p1, p2, p3) ->
            snd (runMockRetry pauseStrategy 5 action)
              `shouldBe` [ActionCall, ThreadDelayCall p1, ActionCall, ThreadDelayCall p2, ActionCall, ThreadDelayCall p3, ActionCall]
      it "for constPause" $ testPause (R.constPause 10) (10, 10, 10)
      it "for linearPause" $ testPause (R.linearPause 10) (10, 20, 30)
      it "for expPause" $ testPause (R.expPause 10) (10, 20, 40)
      it "for customPause" $ testPause (R.customPause (^ (2 :: Int))) (1, 4, 9)

  describe "retryWithOnRetry" $ do
    it "does not call onRetry when action succeeds on the first try" $ do
      runMockRetryWithOnRetry (R.constPause 42) 2 (mockAction (NumFails 0))
        `shouldBe` (Right (), [ActionCall])
    it "calls onRetry before each pause, with number of failed tries and the error" $ do
      runMockRetryWithOnRetry (R.constPause 42) 5 (mockAction (NumFails 2))
        `shouldBe` ( Right (),
                     [ ActionCall,
                       OnRetryCall 1 1,
                       ThreadDelayCall 42,
                       ActionCall,
                       OnRetryCall 2 2,
                       ThreadDelayCall 42,
                       ActionCall
                     ]
                   )
    it "does not call onRetry after the final failed try" $ do
      runMockRetryWithOnRetry (R.constPause 42) 0 (mockAction (NumFails 3))
        `shouldBe` (Left 1, [ActionCall])
      runMockRetryWithOnRetry (R.constPause 42) 2 (mockAction (NumFails 3))
        `shouldBe` ( Left 3,
                     [ ActionCall,
                       OnRetryCall 1 1,
                       ThreadDelayCall 42,
                       ActionCall,
                       OnRetryCall 2 2,
                       ThreadDelayCall 42,
                       ActionCall
                     ]
                   )

runMockRetry :: R.PauseStrategy -> Natural -> MockAction -> (Either TryNumber (), [Event])
runMockRetry pause maxNumRetries action = runState (R.retry pause maxNumRetries action) []

runMockRetryWithOnRetry :: R.PauseStrategy -> Natural -> MockAction -> (Either TryNumber (), [Event])
runMockRetryWithOnRetry pause maxNumRetries action =
  runState (R.retryWithOnRetry pause maxNumRetries onRetry action) []
  where
    onRetry numFailedTries e = modify (++ [OnRetryCall numFailedTries e])

-- | Fails with the number of the try that failed (starting at 1).
type MockAction = MockRetryMonad (Either TryNumber ())

type TryNumber = Int

mockAction :: NumFails -> MockAction
mockAction (NumFails numFails) = do
  events <- get
  let numPreviousTries = length (filter (== ActionCall) events)
  let result =
        if numPreviousTries >= numFails
          then Right ()
          else Left (numPreviousTries + 1)
  modify (++ [ActionCall])
  return result

newtype NumFails = NumFails Int

data Event = ThreadDelayCall Int | ActionCall | OnRetryCall R.NumFailedTries TryNumber
  deriving (Show, Eq)

type MockRetryMonad = State [Event]

instance R.MonadRetry MockRetryMonad where
  rThreadDelay microseconds = modify (++ [ThreadDelayCall microseconds])
