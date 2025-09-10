module Analyzer.Parser.SourcePositionTest where

import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.SourcePosition

spec_SourcePositionTest :: Spec
spec_SourcePositionTest = do
  describe "Analyzer.Parser.SourcePosition" $ do
    describe "calcNextPosition works correctly" $ do
      it "when source is empty" $ do
        calcNextPosition "" (SourcePosition 1 5) `shouldBe` SourcePosition 1 5
      it "when source has no newlines" $ do
        calcNextPosition "foo + bar" (SourcePosition 1 5) `shouldBe` SourcePosition 1 14
        calcNextPosition "app App { }" (SourcePosition 3 1) `shouldBe` SourcePosition 3 12
      it "when source has newlines" $ do
        calcNextPosition "foo\nbar\nbuzz" (SourcePosition 3 4) `shouldBe` SourcePosition 5 5
        calcNextPosition "app App {\n  test: Hi\n}" (SourcePosition 2 1) `shouldBe` SourcePosition 4 2
        calcNextPosition "foo\nbar\n" (SourcePosition 2 1) `shouldBe` SourcePosition 4 1
