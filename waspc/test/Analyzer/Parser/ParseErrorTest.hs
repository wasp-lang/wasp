module Analyzer.Parser.ParseErrorTest where

import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.Token

spec_ParseErrorTest :: Spec
spec_ParseErrorTest = do
  describe "Analyzer.Parser.ParseError" $ do
    let unexpectedCharError = UnexpectedChar '!' (SourcePosition 2 42)
        unexpectedTokenErrorNoSuggestions =
          UnexpectedToken (Token TLCurly (SourcePosition 2 3) "{") []
        unexpectedTokenErrorWithSuggestions =
          UnexpectedToken
            (Token TRCurly (SourcePosition 100 18) "}")
            ["<identifier>", ","]
        quoterDifferentTagsError =
          QuoterDifferentTags
            ("foo", SourcePosition 1 5)
            ("bar", SourcePosition 1 20)
    describe "getErrorMessage returns human readable error message" $ do
      it "for UnexpectedChar error" $ do
        getErrorMessage unexpectedCharError `shouldBe` "Unexpected character: !"
      it "for UnexpectedToken error" $ do
        getErrorMessage unexpectedTokenErrorNoSuggestions
          `shouldBe` "Unexpected token: {"
        getErrorMessage unexpectedTokenErrorWithSuggestions
          `shouldBe` ( "Unexpected token: }\n"
                         ++ "Expected one of the following tokens instead: <identifier> ,"
                     )
      it "for QuoterDifferentTags error" $ do
        getErrorMessage quoterDifferentTagsError
          `shouldBe` "Quoter tags don't match: {=foo ... bar=}"

    describe "getSourcePosition returns correct position" $ do
      it "for UnexpectedChar error" $ do
        getSourcePosition unexpectedCharError `shouldBe` SourcePosition 2 42
      it "for UnexpectedToken error" $ do
        getSourcePosition unexpectedTokenErrorNoSuggestions
          `shouldBe` SourcePosition 2 3
        getSourcePosition unexpectedTokenErrorWithSuggestions
          `shouldBe` SourcePosition 100 18
      it "for QuoterDifferentTags error" $ do
        getSourcePosition quoterDifferentTagsError
          `shouldBe` SourcePosition 1 20
