module Util.JsTest where

import Test.Hspec
import Wasp.Util.Js (makeJsStringLiteral)

spec_makeJsStringLiteral :: Spec
spec_makeJsStringLiteral = do
  it "quotes an ordinary string" $ do
    makeJsStringLiteral "/up" `shouldBe` "\"/up\""

  -- Whatever the user writes as a path or a name ends up in one of these, so a quote or a
  -- backslash has to be escaped instead of ending the literal early.
  it "escapes a double quote" $ do
    makeJsStringLiteral "say \"hi\"" `shouldBe` "\"say \\\"hi\\\"\""

  it "leaves a single quote alone" $ do
    makeJsStringLiteral "O'Brien" `shouldBe` "\"O'Brien\""

  it "escapes a backslash" $ do
    makeJsStringLiteral "/back\\slash" `shouldBe` "\"/back\\\\slash\""

  it "escapes line breaks" $ do
    makeJsStringLiteral "two\nlines\r" `shouldBe` "\"two\\nlines\\r\""
