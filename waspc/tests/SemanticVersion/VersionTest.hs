module SemanticVersion.VersionTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.SemanticVersion.Version

spec_SemanticVersion_Version :: Spec
spec_SemanticVersion_Version = do
  it "show produces valid semver representation of version" $ do
    show (Version 1 (Just 2) (Just 3)) `shouldBe` "1.2.3"
    show (Version 1 (Just 2) Nothing) `shouldBe` "1.2"
    show (Version 1 Nothing Nothing) `shouldBe` "1"

  it "parseVersion" $ do
    parseVersion "1" `shouldBe` Right (Version 1 Nothing Nothing)
    parseVersion "1.2" `shouldBe` Right (Version 1 (Just 2) Nothing)
    parseVersion "1.2.3" `shouldBe` Right (Version 1 (Just 2) (Just 3))
    parseVersion "103.20.35" `shouldBe` Right (Version 103 (Just 20) (Just 35))
    parseVersion "0.1.33" `shouldBe` Right (Version 0 (Just 1) (Just 33))
    parseVersion "1.2.3foobar" `shouldBe` Right (Version 1 (Just 2) (Just 3))
    parseVersion "1.2.3-alpha" `shouldBe` Right (Version 1 (Just 2) (Just 3))
    parseVersion "1.2.3 some other stuff" `shouldBe` Right (Version 1 (Just 2) (Just 3))
    isLeft (parseVersion "1.2.3.") `shouldBe` True
    isLeft (parseVersion "1.2.3.blabla") `shouldBe` True
    isLeft (parseVersion "v1.2.3") `shouldBe` True
    isLeft (parseVersion ".2.3") `shouldBe` True
    isLeft (parseVersion "foo") `shouldBe` True

  it "nextBreakingChangeVersion" $ do
    nextBreakingChangeVersion (FullVersion 1 2 3) `shouldBe` FullVersion 2 0 0
    nextBreakingChangeVersion (FullVersion 0 2 3) `shouldBe` FullVersion 0 3 0
    nextBreakingChangeVersion (FullVersion 0 0 3) `shouldBe` FullVersion 0 0 4
    nextBreakingChangeVersion (Version 1 (Just 2) Nothing) `shouldBe` FullVersion 2 0 0
    nextBreakingChangeVersion (Version 1 Nothing Nothing) `shouldBe` FullVersion 2 0 0

  it "v quasi quoter" $ do
    [v|1.2.3|] `shouldBe` Version 1 (Just 2) (Just 3)
    [v|1.2|] `shouldBe` Version 1 (Just 2) Nothing
    [v|1|] `shouldBe` Version 1 Nothing Nothing
    [v|0.2|] `shouldBe` Version 0 (Just 2) Nothing
    [v|0.0.2|] `shouldBe` Version 0 (Just 0) (Just 2)
