module Project.WaspNpmPackageTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Project.WaspNpmPackage
  ( NpmPackageVersion (..),
    doesVersionMatchDeclaredSpecifier,
    parseNpmPackageVersion,
  )
import qualified Wasp.SemanticVersion as SV

spec_WaspNpmPackage :: Spec
spec_WaspNpmPackage = do
  describe "parseNpmPackageVersion" $ do
    it "parses a release version" $ do
      parseNpmPackageVersion "0.27.0" `shouldBe` Right (NpmPackageVersion (SV.Version 0 27 0) False "0.27.0")

    -- The release workflow publishes RCs with the version of their tag, e.g. `v0.27.0-rc.1`.
    it "parses a release candidate version" $ do
      parseNpmPackageVersion "0.27.0-rc.1" `shouldBe` Right (NpmPackageVersion (SV.Version 0 27 0) True "0.27.0-rc.1")

    it "parses a version with build metadata" $ do
      parseNpmPackageVersion "0.27.0+build.5" `shouldBe` Right (NpmPackageVersion (SV.Version 0 27 0) False "0.27.0+build.5")

    it "rejects invalid versions" $ do
      parseNpmPackageVersion "0.27" `shouldSatisfy` isLeft
      parseNpmPackageVersion "0.27.0-" `shouldSatisfy` isLeft
      parseNpmPackageVersion "0.27.0-rc..1" `shouldSatisfy` isLeft

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
    version :: String -> NpmPackageVersion
    version = either error id . parseNpmPackageVersion
