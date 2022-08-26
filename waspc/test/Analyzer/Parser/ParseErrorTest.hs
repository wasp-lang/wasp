module Analyzer.Parser.ParseErrorTest where

import Analyzer.TestUtil (ctx, pos, rgn, wctx)
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.Token
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

spec_ParseErrorTest :: Spec
spec_ParseErrorTest = do
  describe "Wasp.Analyzer.Parser.ParseError" $ do
    describe "getErrorMessageAndCtx returns a human readable error message and the correct position" $ do
      let unexpectedTokenErrorNoSuggestions =
            UnexpectedToken (rgn (2, 3) (2, 3)) "}" RCurly (TokenSet.fromList [])
      let unexpectedTokenErrorWithSuggestions =
            UnexpectedToken (rgn (2, 3) (2, 3)) "}" RCurly (TokenSet.fromList [LCurly])
      let unexpectedTokenErrorWithManySuggestions =
            UnexpectedToken (rgn (2, 3) (2, 3)) "}" RCurly (TokenSet.fromList [LCurly, Identifier])
      let unexpectedEofErrorNoSuggestions =
            UnexpectedEOF (pos 2 3) (TokenSet.fromList [])
      let unexpectedEofErrorWithSuggestions =
            UnexpectedEOF (pos 2 3) (TokenSet.fromList [LCurly])
      let quoterDifferentTagsError =
            QuoterDifferentTags (wctx (2, 3) (2, 4) "a") (wctx (2, 5) (2, 6) "b")
      let tupleTooFewValuesError =
            TupleTooFewValues (rgn (2, 3) (2, 5)) 1
      let missingSyntaxError =
            MissingSyntax (pos 2 3) "comma"

      it "UnexpectedToken" $ do
        getErrorMessageAndCtx unexpectedTokenErrorNoSuggestions
          `shouldBe` ("Unexpected token: }", ctx (2, 3) (2, 3))
        getErrorMessageAndCtx unexpectedTokenErrorWithSuggestions
          `shouldBe` ( "Unexpected token: }\n"
                         ++ "Expected one of the following tokens instead: '{'",
                       ctx (2, 3) (2, 3)
                     )
        getErrorMessageAndCtx unexpectedTokenErrorWithManySuggestions
          `shouldBe` ( "Unexpected token: }\n"
                         ++ "Expected one of the following tokens instead: '{',<identifier>",
                       ctx (2, 3) (2, 3)
                     )

      it "UnexpecteEOF" $ do
        getErrorMessageAndCtx unexpectedEofErrorNoSuggestions
          `shouldBe` ( "Unexpected end of file",
                       ctx (2, 3) (2, 3)
                     )
        getErrorMessageAndCtx unexpectedEofErrorWithSuggestions
          `shouldBe` ( "Unexpected end of file\n"
                         ++ "Expected one of the following tokens instead: '{'",
                       ctx (2, 3) (2, 3)
                     )

      it "QuoterDifferentTags" $ do
        getErrorMessageAndCtx quoterDifferentTagsError
          `shouldBe` ("Quoter tags don't match: {=a ... b=}", ctx (2, 3) (2, 6))

      it "TupleTooFewValues" $ do
        getErrorMessageAndCtx tupleTooFewValuesError
          `shouldBe` ( "Tuple only contains 1 values, but it must contain at least 2 values",
                       ctx (2, 3) (2, 5)
                     )

      it "MissingSyntax" $ do
        getErrorMessageAndCtx missingSyntaxError
          `shouldBe` ("Missing expected comma", ctx (2, 3) (2, 3))
