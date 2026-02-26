module SemanticVersion.PartialVersionTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_PartialVersion :: Spec
spec_SemanticVersion_PartialVersion = do
  describe "show" $ do
    it "produces valid semver representation" $ do
      show (Full 1 2 3) `shouldBe` "1.2.3"
      show (MajorMinor 1 2) `shouldBe` "1.2"
      show (Major 1) `shouldBe` "1"
      show Any `shouldBe` "*"

  describe "parsePartialVersion" $ do
    it "parses full versions" $ do
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

    it "parses Any wildcards" $ do
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

  describe "pv quasiquoter" $ do
    it "creates Full versions" $ do
      [pv|1.2.3|] `shouldBe` Full 1 2 3
      [pv|0.0.0|] `shouldBe` Full 0 0 0

    it "creates MajorMinor versions" $ do
      [pv|1.2|] `shouldBe` MajorMinor 1 2
      [pv|0.0|] `shouldBe` MajorMinor 0 0

    it "creates Major versions" $ do
      [pv|1|] `shouldBe` Major 1
      [pv|0|] `shouldBe` Major 0

    it "creates Any from wildcards" $ do
      [pv|*|] `shouldBe` Any

  describe "fromVersion" $ do
    it "converts Version to Full PartialVersion" $ do
      fromVersion (Version 1 2 3) `shouldBe` Full 1 2 3
      fromVersion (Version 0 0 0) `shouldBe` Full 0 0 0

  describe "toLowerBound" $ do
    it "returns version with missing components as 0" $ do
      toLowerBound (Full 1 2 3) `shouldBe` Version 1 2 3
      toLowerBound (MajorMinor 1 2) `shouldBe` Version 1 2 0
      toLowerBound (Major 1) `shouldBe` Version 1 0 0
      toLowerBound Any `shouldBe` Version 0 0 0

  describe "toUpperBound" $ do
    it "returns Inclusive for full versions" $ do
      toUpperBound (Full 1 2 3) `shouldBe` Inclusive (Version 1 2 3)

    it "returns Exclusive of next increment for partial versions" $ do
      toUpperBound (MajorMinor 1 2) `shouldBe` Exclusive (Version 1 3 0)
      toUpperBound (Major 1) `shouldBe` Exclusive (Version 2 0 0)

    it "returns Inf for Any" $ do
      toUpperBound Any `shouldBe` Inf

  describe "toTildeUpperBound" $ do
    it "allows patch-level changes if minor is specified" $ do
      toTildeUpperBound (Full 1 2 3) `shouldBe` Exclusive (Version 1 3 0)
      toTildeUpperBound (MajorMinor 1 2) `shouldBe` Exclusive (Version 1 3 0)
      toTildeUpperBound (Major 1) `shouldBe` Exclusive (Version 2 0 0)
      toTildeUpperBound Any `shouldBe` Inf

  describe "toCaretUpperBound" $ do
    it "allows changes that do not modify leftmost non-zero digit" $ do
      -- \^1.2.3 := >=1.2.3 <2.0.0
      toCaretUpperBound (Full 1 2 3) `shouldBe` Exclusive (Version 2 0 0)
      -- \^0.2.3 := >=0.2.3 <0.3.0
      toCaretUpperBound (Full 0 2 3) `shouldBe` Exclusive (Version 0 3 0)
      -- \^0.0.3 := >=0.0.3 <0.0.4
      toCaretUpperBound (Full 0 0 3) `shouldBe` Exclusive (Version 0 0 4)
      -- \^1.2 := >=1.2.0 <2.0.0
      toCaretUpperBound (MajorMinor 1 2) `shouldBe` Exclusive (Version 2 0 0)
      -- \^0.0 := >=0.0.0 <0.1.0
      toCaretUpperBound (MajorMinor 0 0) `shouldBe` Exclusive (Version 0 1 0)
      -- \^0.2 := >=0.2.0 <0.3.0
      toCaretUpperBound (MajorMinor 0 2) `shouldBe` Exclusive (Version 0 3 0)
      -- \^1 := >=1.0.0 <2.0.0
      toCaretUpperBound (Major 1) `shouldBe` Exclusive (Version 2 0 0)
      -- \^0 := >=0.0.0 <1.0.0
      toCaretUpperBound (Major 0) `shouldBe` Exclusive (Version 1 0 0)
      toCaretUpperBound Any `shouldBe` Inf
