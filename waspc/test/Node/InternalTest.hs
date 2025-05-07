module Node.InternalTest where

import Test.Tasty.Hspec
import Wasp.Node.Internal (parseVersionFromCommandOutput)
import Wasp.SemanticVersion.Version (Version (..))

spec_NodeInternal :: Spec
spec_NodeInternal =
  describe "parseVersionFromCommandOutput" $ do
    it "parses a version when the version string is at the end" $ do
      let output = "Some informational output...\nVersion: 20.3.1"
      parseVersionFromCommandOutput output `shouldBe` Right (Version 20 3 1)

    it "parses a version when the version string is in the middle" $ do
      let output = "Starting up v20.3.1 and initializing modules"
      parseVersionFromCommandOutput output `shouldBe` Right (Version 20 3 1)

    it "parses a version when there is only version string" $ do
      let output = "20.3.1"
      parseVersionFromCommandOutput output `shouldBe` Right (Version 20 3 1)

    it "fails when no valid version is found" $ do
      let output = "This output has no semantic version info!"
      case parseVersionFromCommandOutput output of
        Right _ -> expectationFailure "Expected parse failure but got a version."
        Left err -> err `shouldContain` "Wasp failed to parse version from string"
