module Generator.CommonTest where

import Test.Hspec
import Wasp.Generator.Common (makeJsArrayFromHaskellList, makeJsStringLiteral)

spec_GeneratorCommonTest :: Spec
spec_GeneratorCommonTest = do
  describe "makeJsArrayFromHaskellList" $ do
    it "creates an empty JS array from empty Haskell list" $ do
      makeJsArrayFromHaskellList []
        `shouldBe` "[]"

    it "creates an array of strings from Haskell list of strings" $ do
      makeJsArrayFromHaskellList ["one", "two", "three"]
        `shouldBe` "[\"one\", \"two\", \"three\"]"

  describe "makeJsStringLiteral" $ do
    it "quotes an ordinary string" $ do
      makeJsStringLiteral "/health"
        `shouldBe` "\"/health\""

    -- Whatever the user writes as an `api` path ends up in one of these, so a quote or a
    -- backslash has to be escaped instead of ending the literal early.
    it "escapes a double quote" $ do
      makeJsStringLiteral "/say\"hi"
        `shouldBe` "\"/say\\\"hi\""

    it "escapes a backslash" $ do
      makeJsStringLiteral "/back\\slash"
        `shouldBe` "\"/back\\\\slash\""

    it "escapes a newline" $ do
      makeJsStringLiteral "/two\nlines"
        `shouldBe` "\"/two\\nlines\""
