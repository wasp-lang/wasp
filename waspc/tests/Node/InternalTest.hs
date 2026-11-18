module Node.InternalTest where

import Test.Hspec
import Wasp.Node.Internal (parseVersionFromCommandOutput)
import Wasp.SemanticVersion.Version (v)

spec_NodeInternal :: Spec
spec_NodeInternal =
  describe "parseVersionFromCommandOutput" $ do
    it "parses a version when the version string is at the end" $ do
      let output = "Some informational output...\nVersion: 20.3.1"
      parseVersionFromCommandOutput output `shouldBe` Right [v|20.3.1|]

    it "parses a version when the version string is in the middle" $ do
      let output = "Starting up v20.3.1 and initializing modules"
      parseVersionFromCommandOutput output `shouldBe` Right [v|20.3.1|]

    it "parses a version when there is only version string" $ do
      let output = "20.3.1"
      parseVersionFromCommandOutput output `shouldBe` Right [v|20.3.1|]

    it "parses a pre-release version" $ do
      let output = "v26.0.0-nightly20260101a1b2c3d4e5"
      parseVersionFromCommandOutput output `shouldBe` Right [v|26.0.0-nightly20260101a1b2c3d4e5|]

    it "parses a version with build" $ do
      let output = "11.11.0+build.5\n"
      show <$> parseVersionFromCommandOutput output `shouldBe` Right "11.11.0+build.5"

    it "fails when no valid version is found" $ do
      let output = "This output has no semantic version info!"
      case parseVersionFromCommandOutput output of
        Right _ -> expectationFailure "Expected parse failure but got a version."
        Left err -> err `shouldContain` "Wasp failed to parse version from string"
