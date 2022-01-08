module Analyzer.Parser.TokenTest where

import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.SourcePosition
import Wasp.Analyzer.Parser.Token

spec_TokenTest :: Spec
spec_TokenTest = do
  describe "Analyzer.Parser.Token" $ do
    it "calcTokenEndPos works correctly" $ do
      calcTokenEndPos (Token (TIdentifier "foo") (SourcePosition 2 3) "foo") `shouldBe` SourcePosition 2 5
      calcTokenEndPos (Token (TString "foo") (SourcePosition 5 10) "\"foo\"") `shouldBe` SourcePosition 5 14
      calcTokenEndPos (Token TLSquare (SourcePosition 2 3) "[") `shouldBe` SourcePosition 2 3
      calcTokenEndPos (Token TEOF (SourcePosition 2 3) "") `shouldBe` SourcePosition 2 3
