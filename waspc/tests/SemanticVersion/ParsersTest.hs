module SemanticVersion.ParsersTest where

import Data.Either (isLeft)
import Test.Hspec
import Text.Parsec (parse)
import Wasp.SemanticVersion.Parsers (naturalNumberParser)

spec_SemanticVersion_Parsers :: Spec
spec_SemanticVersion_Parsers = do
  describe "naturalNumberParser" $ do
    it "parses zero" $ do
      parseNatural "0" `shouldBe` Right 0

    it "parses natural numbers" $ do
      parseNatural "1" `shouldBe` Right 1
      parseNatural "5" `shouldBe` Right 5
      parseNatural "9" `shouldBe` Right 9
      parseNatural "10" `shouldBe` Right 10
      parseNatural "42" `shouldBe` Right 42
      parseNatural "123" `shouldBe` Right 123
      parseNatural "99999" `shouldBe` Right 99999

    it "parses natural numbers with trailing content" $ do
      parseNatural "10.asda" `shouldBe` Right 10
      parseNatural "10-10" `shouldBe` Right 10

    it "rejects leading zeros" $ do
      isLeft (parseNatural "01") `shouldBe` True
      isLeft (parseNatural "00") `shouldBe` True
      isLeft (parseNatural "0123") `shouldBe` True

    it "rejects invalid formats" $ do
      isLeft (parseNatural "") `shouldBe` True
      isLeft (parseNatural "abc") `shouldBe` True
      isLeft (parseNatural "-1") `shouldBe` True
      isLeft (parseNatural ".123") `shouldBe` True
  where
    parseNatural = parse naturalNumberParser ""
