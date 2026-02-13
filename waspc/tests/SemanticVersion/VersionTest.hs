module SemanticVersion.VersionTest where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Wasp.SemanticVersion.Version

spec_SemanticVersion_Version :: Spec
spec_SemanticVersion_Version = do
  it "show produces valid semver representation of version" $ do
    show (Version 1 2 3) `shouldBe` "1.2.3"

  it "parseVersion succeeds for valid full versions" $ do
    parseVersion "1.2.3" `shouldBe` Right (Version 1 2 3)
    parseVersion "103.20.35" `shouldBe` Right (Version 103 20 35)
    parseVersion "0.1.33" `shouldBe` Right (Version 0 1 33)
    parseVersion "0.0.0" `shouldBe` Right (Version 0 0 0)

  it "parseVersion accepts versions with trailing content" $ do
    isRight (parseVersion "1.2.3foobar") `shouldBe` True
    isRight (parseVersion "1.2.3-alpha") `shouldBe` True
    isRight (parseVersion "1.2.3 some other stuff") `shouldBe` True

  it "parseVersion fails for partial versions" $ do
    isLeft (parseVersion "1") `shouldBe` True
    isLeft (parseVersion "1.2") `shouldBe` True

  it "parseVersion fails for invalid formats" $ do
    -- Note: parser allows trailing content, so "1.2.3." and "1.2.3.4" succeed (parsing "1.2.3")
    isLeft (parseVersion "v1.2.3") `shouldBe` True
    isLeft (parseVersion ".2.3") `shouldBe` True
    isLeft (parseVersion "foo") `shouldBe` True
    isLeft (parseVersion "") `shouldBe` True

  it "v quasi quoter" $ do
    [v|1.2.3|] `shouldBe` Version 1 2 3
    [v|0.0.0|] `shouldBe` Version 0 0 0
    [v|103.20.35|] `shouldBe` Version 103 20 35
