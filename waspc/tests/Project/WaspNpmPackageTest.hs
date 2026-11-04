module Project.WaspNpmPackageTest where

import Test.Hspec
import Wasp.Project.WaspNpmPackage (doesVersionMatchDeclaredSpecifier)
import qualified Wasp.SemanticVersion as SV

spec_WaspNpmPackage :: Spec
spec_WaspNpmPackage = do
  describe "doesVersionMatchDeclaredSpecifier" $ do
    it "checks release versions against a range" $ do
      doesVersionMatchDeclaredSpecifier "^0.27.0" (version "0.27.3") `shouldBe` True
      doesVersionMatchDeclaredSpecifier "0.27.0" (version "0.27.1") `shouldBe` False

    it "doesn't match a release candidate against a range, like npm" $ do
      doesVersionMatchDeclaredSpecifier "0.27.0" (version "0.27.0-rc.1") `shouldBe` False
      doesVersionMatchDeclaredSpecifier "^0.27.0" (version "0.27.0-rc.1") `shouldBe` False

    it "matches an exact release candidate only against itself" $ do
      doesVersionMatchDeclaredSpecifier "0.27.0-rc.1" (version "0.27.0-rc.1") `shouldBe` True
      doesVersionMatchDeclaredSpecifier "0.27.0-rc.1" (version "0.27.0-rc.2") `shouldBe` False
      doesVersionMatchDeclaredSpecifier "0.27.0-rc.1" (version "0.27.0") `shouldBe` False

    it "doesn't constrain specifiers that aren't versions" $ do
      doesVersionMatchDeclaredSpecifier "file:../wasp-wasp-0.27.0.tgz" (version "0.27.0-rc.1") `shouldBe` True
  where
    version :: String -> SV.Version
    version = either (error . show) id . SV.parseVersion
