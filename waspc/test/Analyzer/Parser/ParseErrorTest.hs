module Analyzer.Parser.ParseErrorTest where

import Analyzer.TestUtil (ctx, pos)
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.Token

spec_ParseErrorTest :: Spec
spec_ParseErrorTest = do
  describe "Analyzer.Parser.ParseError" $ do
    describe "getErrorMessageAndCtx returns a human readable error message and the correct position" $ do
      let unexpectedCharError = UnexpectedChar '!' (pos 2 42)
          unexpectedTokenErrorNoSuggestions =
            UnexpectedToken (Token TLCurly (pos 2 3) "{") []
          unexpectedTokenErrorWithSuggestions =
            UnexpectedToken
              (Token TRCurly (pos 100 18) "}")
              ["<identifier>", ","]
          quoterDifferentTagsError =
            QuoterDifferentTags
              ("foo", pos 1 5)
              ("bar", pos 1 20)

      it "for UnexpectedChar error" $ do
        getErrorMessageAndCtx unexpectedCharError `shouldBe` ("Unexpected character: !", ctx 2 42)

      it "for UnexpectedToken error" $ do
        getErrorMessageAndCtx unexpectedTokenErrorNoSuggestions
          `shouldBe` ("Unexpected token: {", ctx 2 3)
        getErrorMessageAndCtx unexpectedTokenErrorWithSuggestions
          `shouldBe` ( "Unexpected token: }\n"
                         ++ "Expected one of the following tokens instead: <identifier> ,",
                       ctx 100 18
                     )

      it "for QuoterDifferentTags error" $ do
        getErrorMessageAndCtx quoterDifferentTagsError
          `shouldBe` ("Quoter tags don't match: {=foo ... bar=}", ctx 1 20)
