module SemanticVersion.ParsersTest where

import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Parsers (naturalNumberParser)

spec_SemanticVersion_Parsers :: Spec
spec_SemanticVersion_Parsers = do
  describe "naturalNumberParser" $ do
    let parseNatural = P.parse naturalNumberParser ""
        strictParseNatural = P.parse (naturalNumberParser <* P.eof) ""

    it "parses zero" $ do
      strictParseNatural "0" `shouldBe` Right 0

    it "parses natural numbers" $ do
      strictParseNatural "1" `shouldBe` Right 1
      strictParseNatural "5" `shouldBe` Right 5
      strictParseNatural "9" `shouldBe` Right 9
      strictParseNatural "10" `shouldBe` Right 10
      strictParseNatural "42" `shouldBe` Right 42
      strictParseNatural "123" `shouldBe` Right 123
      strictParseNatural "99999" `shouldBe` Right 99999

    it "parses natural numbers with trailing content" $ do
      parseNatural "10.asda" `shouldBe` Right 10
      parseNatural "10-10" `shouldBe` Right 10

    it "rejects leading zeros" $ do
      isLeft (strictParseNatural "01") `shouldBe` True
      isLeft (strictParseNatural "00") `shouldBe` True
      isLeft (strictParseNatural "0123") `shouldBe` True

    it "rejects invalid formats" $ do
      isLeft (strictParseNatural "") `shouldBe` True
      isLeft (strictParseNatural "123.55") `shouldBe` True
      isLeft (strictParseNatural "1/2") `shouldBe` True
      isLeft (strictParseNatural "abc") `shouldBe` True
      isLeft (strictParseNatural "-1") `shouldBe` True
      isLeft (strictParseNatural ".123") `shouldBe` True
