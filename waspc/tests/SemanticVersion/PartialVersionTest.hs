module SemanticVersion.PartialVersionTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.SemanticVersion.PartialVersion

spec_SemanticVersion_PartialVersion :: Spec
spec_SemanticVersion_PartialVersion = do
  it "show" $ do
    show (Full 1 2 3) `shouldBe` "1.2.3"
    show (MajorMinor 1 2) `shouldBe` "1.2"
    show (Major 1) `shouldBe` "1"
    show Any `shouldBe` "*"

  describe "parsePartialVersion" $ do
    it "parses major.minor.patch versions" $ do
      parsePartialVersion "1.2.3" `shouldBe` Right (Full 1 2 3)
      parsePartialVersion "0.0.0" `shouldBe` Right (Full 0 0 0)
      parsePartialVersion "103.20.35" `shouldBe` Right (Full 103 20 35)

    it "parses major.minor versions" $ do
      parsePartialVersion "1.2" `shouldBe` Right (MajorMinor 1 2)
      parsePartialVersion "0.0" `shouldBe` Right (MajorMinor 0 0)
      parsePartialVersion "10.20" `shouldBe` Right (MajorMinor 10 20)

    it "parses major versions" $ do
      parsePartialVersion "1" `shouldBe` Right (Major 1)
      parsePartialVersion "0" `shouldBe` Right (Major 0)
      parsePartialVersion "123" `shouldBe` Right (Major 123)

    it "parses sole wildcards" $ do
      parsePartialVersion "*" `shouldBe` Right Any
      parsePartialVersion "x" `shouldBe` Right Any
      parsePartialVersion "X" `shouldBe` Right Any

    it "parses major with minor wildcard" $ do
      parsePartialVersion "1.x" `shouldBe` Right (Major 1)
      parsePartialVersion "1.X" `shouldBe` Right (Major 1)
      parsePartialVersion "1.*" `shouldBe` Right (Major 1)

    it "parses major with minor and patch wildcards" $ do
      parsePartialVersion "1.x.x" `shouldBe` Right (Major 1)
      parsePartialVersion "1.X.X" `shouldBe` Right (Major 1)
      parsePartialVersion "1.*.*" `shouldBe` Right (Major 1)

    it "parses major.minor with patch wildcard" $ do
      parsePartialVersion "1.2.x" `shouldBe` Right (MajorMinor 1 2)
      parsePartialVersion "1.2.X" `shouldBe` Right (MajorMinor 1 2)
      parsePartialVersion "1.2.*" `shouldBe` Right (MajorMinor 1 2)

    it "rejects invalid formats" $ do
      isLeft (parsePartialVersion "") `shouldBe` True
      isLeft (parsePartialVersion "v1.2.3") `shouldBe` True
      isLeft (parsePartialVersion ".2.3") `shouldBe` True
      isLeft (parsePartialVersion "foo") `shouldBe` True

    it "rejects wildcard in major position with numeric minor" $ do
      isLeft (parsePartialVersion "x.2") `shouldBe` True
      isLeft (parsePartialVersion "*.2.3") `shouldBe` True

    it "rejects wildcard in minor position with numeric patch" $ do
      isLeft (parsePartialVersion "1.x.3") `shouldBe` True
      isLeft (parsePartialVersion "1.*.3") `shouldBe` True

  describe "pv quasi quoter" $ do
    it "creates major.minor.patch versions" $ do
      [pv|1.2.3|] `shouldBe` Full 1 2 3
      [pv|0.0.0|] `shouldBe` Full 0 0 0

    it "creates major.minor versions" $ do
      [pv|1.2|] `shouldBe` MajorMinor 1 2
      [pv|0.0|] `shouldBe` MajorMinor 0 0

    it "creates major versions" $ do
      [pv|1|] `shouldBe` Major 1
      [pv|0|] `shouldBe` Major 0

    it "creates wildcard versions from wildcards" $ do
      [pv|*|] `shouldBe` Any
      [pv|x|] `shouldBe` Any
      [pv|X|] `shouldBe` Any
