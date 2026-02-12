module Wasp.Cli.Command.Build.DockerBuildContextTest where

import Test.Hspec
import Wasp.Cli.Command.Build.DockerBuildContext (isWaspConfigPackageLocation)

spec_isWaspConfigPackageLocation :: Spec
spec_isWaspConfigPackageLocation = do
  describe "isWaspConfigPackageLocation" $ do
    it "matches POSIX-style package-lock package locations" $ do
      isWaspConfigPackageLocation "node_modules/wasp-config" `shouldBe` True

    it "matches Windows-style package-lock package locations" $ do
      isWaspConfigPackageLocation "node_modules\\wasp-config" `shouldBe` True

    it "matches a bare wasp-config package location" $ do
      isWaspConfigPackageLocation "wasp-config" `shouldBe` True

    it "does not match similarly named packages" $ do
      isWaspConfigPackageLocation "node_modules/wasp-config-extra" `shouldBe` False
