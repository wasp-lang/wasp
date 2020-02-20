module Util.FibTest where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Util.Fib

spec_fibonacci :: Spec
spec_fibonacci = do
  describe "Fibonacci" $ do
    it "fibonacci element #0 is 0" $ do
      fibonacci 0 `shouldBe` 0
    it "fibonacci element #1 is 1" $ do
      fibonacci 1 `shouldBe` 1
    it "fibonacci element #2 is 1" $ do
      fibonacci 2 `shouldBe` 1
    it "fibonacci element #3 is 2" $ do
      fibonacci 3 `shouldBe` 2

-- NOTE: Most likely not the best way to write QuickCheck test, I just did this in order
--   to get something working as an example.
prop_fibonacci :: Property
prop_fibonacci = forAll (choose (0, 10)) $ testFibSequence
  where
    testFibSequence :: Int -> Bool
    testFibSequence x = (fibonacci x) + (fibonacci (x + 1)) == fibonacci (x + 2)
