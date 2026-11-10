module Project.WaspNpmPackageTest where

import Test.Hspec
import Wasp.Project.WaspNpmPackage (doesVersionMatchDeclaredVersion)
import qualified Wasp.SemanticVersion as SV

spec_WaspNpmPackage :: Spec
spec_WaspNpmPackage = do
  describe "doesVersionMatchDeclaredVersion" $ do
    it "checks release versions against a range" $ do
      doesVersionMatchDeclaredVersion "^0.27.0" (version "0.27.3") `shouldBe` True
      doesVersionMatchDeclaredVersion "0.27.0" (version "0.27.1") `shouldBe` False

    it "doesn't match a release candidate against a range, like npm" $ do
      doesVersionMatchDeclaredVersion "0.27.0" (version "0.27.0-rc.1") `shouldBe` False
      doesVersionMatchDeclaredVersion "^0.27.0" (version "0.27.0-rc.1") `shouldBe` False

    it "matches an exact release candidate only against itself" $ do
      doesVersionMatchDeclaredVersion "0.27.0-rc.1" (version "0.27.0-rc.1") `shouldBe` True
      doesVersionMatchDeclaredVersion "0.27.0-rc.1" (version "0.27.0-rc.2") `shouldBe` False
      doesVersionMatchDeclaredVersion "0.27.0-rc.1" (version "0.27.0") `shouldBe` False

    it "matches a release candidate against a range that names one of the same version" $ do
      doesVersionMatchDeclaredVersion "^0.27.0-rc.1" (version "0.27.0-rc.2") `shouldBe` True

    it "doesn't constrain declared versions that aren't ranges" $ do
      doesVersionMatchDeclaredVersion "file:../wasp-wasp-0.27.0.tgz" (version "0.27.0-rc.1") `shouldBe` True
  where
    version :: String -> SV.Version
    version = either (error . show) id . SV.parseVersion
