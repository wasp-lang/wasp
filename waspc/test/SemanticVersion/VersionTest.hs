module SemanticVersion.VersionTest where

import Data.Either (isLeft)
import Test.Tasty.Hspec
import Wasp.SemanticVersion.Version

spec_SemanticVersion_Version :: Spec
spec_SemanticVersion_Version = do
  it "show produces valid semver representation of version" $ do
    show (Version 1 2 3) `shouldBe` "1.2.3"

  it "parseVersion" $ do
    parseVersion "1" `shouldBe` Right (Version 1 0 0)
    parseVersion "1.2" `shouldBe` Right (Version 1 2 0)
    parseVersion "1.2.3" `shouldBe` Right (Version 1 2 3)
    parseVersion "103.20.35" `shouldBe` Right (Version 103 20 35)
    parseVersion "0.1.33" `shouldBe` Right (Version 0 1 33)
    parseVersion "1.2.3foobar" `shouldBe` Right (Version 1 2 3)
    parseVersion "1.2.3-alpha" `shouldBe` Right (Version 1 2 3)
    parseVersion "1.2.3 some other stuff" `shouldBe` Right (Version 1 2 3)
    isLeft (parseVersion "1.2.3.") `shouldBe` True
    isLeft (parseVersion "1.2.3.blabla") `shouldBe` True
    isLeft (parseVersion "v1.2.3") `shouldBe` True
    isLeft (parseVersion ".2.3") `shouldBe` True
    isLeft (parseVersion "foo") `shouldBe` True

  it "v quasi quoter" $ do
    [v|1.2.3|] `shouldBe` Version 1 2 3
    [v|1 |] `shouldBe` Version 1 0 0
    [v|0.2|] `shouldBe` Version 0 2 0
