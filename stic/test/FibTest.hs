module FibTest where

import qualified Test.Tasty
import Test.Tasty.Hspec

import Lib

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

-- TODO: Ok so tasty discover works. Can I now somehow use it to have tests in the src/ dir?

-- TODO: Write in README little bit about tasty-discover and give link to it so that people know how to write new tests.
