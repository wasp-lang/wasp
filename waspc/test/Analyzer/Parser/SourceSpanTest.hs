module Analyzer.Parser.SourceSpanTest where

import Test.QuickCheck
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (SourceSpan), spansOverlap)

spec_SourceSpanTest :: Spec
spec_SourceSpanTest = do
  describe "Analyzer.Parser.SourceSpan" $ do
    describe "spansOverlap works" $ do
      it "when first is before second" $ do
        spansOverlap (SourceSpan 0 5) (SourceSpan 10 15) `shouldBe` False
      it "when first ends right before second starts" $ do
        spansOverlap (SourceSpan 0 5) (SourceSpan 5 10) `shouldBe` False
      it "when first overlaps second on its left edge" $ do
        spansOverlap (SourceSpan 0 5) (SourceSpan 4 10) `shouldBe` True
      it "when first is second" $ do
        spansOverlap (SourceSpan 0 5) (SourceSpan 0 5) `shouldBe` True
      it "when first overlaps second on its right edge" $ do
        spansOverlap (SourceSpan 4 10) (SourceSpan 0 5) `shouldBe` True
      it "when second is zero-width" $ do
        spansOverlap (SourceSpan 0 5) (SourceSpan 2 2) `shouldBe` True
      it "is commutative" $ do
        property $ \s0 e0 s1 e1 ->
          let first = SourceSpan s0 e0
              second = SourceSpan s1 e1
           in spansOverlap first second == spansOverlap second first
