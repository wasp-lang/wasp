module Util.IO.RetryTest where

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.Util.IO.Retry as R

spec_RetryTest :: Spec
spec_RetryTest = do
  describe "retry" $ do
    it "runs action only once if it succeeds on the first try" $ do
      -- TODO
      True `shouldBe` True
