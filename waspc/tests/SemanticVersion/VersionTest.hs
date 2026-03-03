module SemanticVersion.VersionTest where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Wasp.SemanticVersion.Version

spec_SemanticVersion_Version :: Spec
spec_SemanticVersion_Version = do
  it "show" $ do
    show (Version 1 2 3) `shouldBe` "1.2.3"
    show (Version 0 2 3) `shouldBe` "0.2.3"
    show (Version 0 0 3) `shouldBe` "0.0.3"
    show (Version 0 0 0) `shouldBe` "0.0.0"

  describe "parserVersion" $ do
    it "parses full versions" $ do
      parseVersion "1.2.3" `shouldBe` Right (Version 1 2 3)
      parseVersion "103.20.35" `shouldBe` Right (Version 103 20 35)
      parseVersion "0.1.33" `shouldBe` Right (Version 0 1 33)
      parseVersion "0.0.0" `shouldBe` Right (Version 0 0 0)
      parseVersion "1.0.1" `shouldBe` Right (Version 1 0 1)

    it "rejects partial versions" $ do
      isLeft (parseVersion "1") `shouldBe` True
      isLeft (parseVersion "1.2") `shouldBe` True

    -- TODO: enable once we add pre-release and build support
    -- it "parseVersion succeeds for valid pre-release versions" $ do
    --   parseVersion "1.2.3-123" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3-123.123" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3-abc" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3-abc.123.abc" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3---.--.-" `shouldBe` Right (Version 1 2 3)

    -- it "parseVersion fails for invalid pre-release versions" $ do
    --   parseVersion "1.2.3-" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3-123." `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3-?" `shouldBe` Right (Version 1 2 3)

    -- it "parseVersion succeeds for valid build versions" $ do
    --   parseVersion "1.2.3+123" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3+abc" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3+123.abc" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3+abc.123.abc" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3+--.--.-" `shouldBe` Right (Version 1 2 3)

    -- it "parseVersion fails for invalid build versions" $ do
    --   parseVersion "1.2.3+" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3+123." `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3+?" `shouldBe` Right (Version 1 2 3)

    -- it "parseVersion parses pre-release and build order properly" $ do
    --   parseVersion "1.2.3-123+abc" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3-123.abc+abc.123" `shouldBe` Right (Version 1 2 3)
    --   parseVersion "1.2.3----.---+---" `shouldBe` Right (Version 1 2 3)
    --   isLeft (parseVersion "1.2.3+123-abc") `shouldBe` True
    --   isLeft (parseVersion "1.2.3-123+") `shouldBe` True

    it "parses full versions with trailing content" $ do
      isRight (parseVersion "1.2.3.4.5.6.7.8.9.0") `shouldBe` True
      isRight (parseVersion "1.2.3foobar") `shouldBe` True
      isRight (parseVersion "1.2.3 some other stuff") `shouldBe` True

    it "rejects invalid formats" $ do
      isLeft (parseVersion "01.2.3") `shouldBe` True
      isLeft (parseVersion "1.02.3") `shouldBe` True
      isLeft (parseVersion "1.2.03") `shouldBe` True
      isLeft (parseVersion "v1.2.3") `shouldBe` True
      isLeft (parseVersion ".2.3") `shouldBe` True
      isLeft (parseVersion "foo") `shouldBe` True
      isLeft (parseVersion "") `shouldBe` True

  it "v quasi quoter" $ do
    [v|1.2.3|] `shouldBe` Version 1 2 3
    [v|0.0.0|] `shouldBe` Version 0 0 0
    [v|103.20.35|] `shouldBe` Version 103 20 35
