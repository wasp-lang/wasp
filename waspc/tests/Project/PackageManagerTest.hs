module Project.PackageManagerTest where

import Test.Hspec
import Wasp.Project.PackageManager

spec_choosePackageManager :: Spec
spec_choosePackageManager = do
  describe "choosePackageManager" $ do
    it "is npm when the project says nothing" $ do
      choose Nothing [] `shouldBe` Right Npm

    it "goes by the lockfile" $ do
      choose Nothing ["package-lock.json"] `shouldBe` Right Npm
      choose Nothing ["pnpm-lock.yaml"] `shouldBe` Right Pnpm
      choose Nothing ["bun.lock"] `shouldBe` Right Bun

    it "takes a pnpm workspace root without a lockfile for pnpm" $ do
      choose Nothing ["pnpm-workspace.yaml"] `shouldBe` Right Pnpm

    it "prefers another package manager's lockfile to a leftover one of npm" $ do
      choose Nothing ["package-lock.json", "pnpm-lock.yaml"] `shouldBe` Right Pnpm
      choose Nothing ["package-lock.json", "bun.lock"] `shouldBe` Right Bun

    it "prefers the `packageManager` field to a lockfile" $ do
      choose (Just "bun") ["pnpm-lock.yaml"] `shouldBe` Right Bun

    it "rejects a package manager that Wasp doesn't work with" $ do
      choose (Just "yarn") [] `shouldBe` Left (UnsupportedPackageManagerError "yarn")
      choose Nothing ["yarn.lock"] `shouldBe` Left (UnsupportedPackageManagerError "yarn")
  where
    choose fieldName markerFileNames =
      choosePackageManager
        PackageManagerEvidence
          { packageManagerFieldName = fieldName,
            markerFileNamesInNearestDir = markerFileNames
          }

spec_packageManagerCommands :: Spec
spec_packageManagerCommands = do
  describe "makeAddPackagesCommand" $ do
    it "adds runtime packages" $ do
      makeAddPackagesCommand Npm Runtime ["lucia@3", "arctic@1"] `shouldBe` "npm install lucia@3 arctic@1"
      makeAddPackagesCommand Pnpm Runtime ["lucia@3"] `shouldBe` "pnpm add lucia@3"
      makeAddPackagesCommand Bun Runtime ["lucia@3"] `shouldBe` "bun add lucia@3"

    it "adds development packages" $ do
      makeAddPackagesCommand Npm Development ["vitest"] `shouldBe` "npm install --save-dev vitest"
      makeAddPackagesCommand Pnpm Development ["vitest"] `shouldBe` "pnpm add --save-dev vitest"
      makeAddPackagesCommand Bun Development ["vitest"] `shouldBe` "bun add --dev vitest"

  describe "makeRunScriptCommand" $ do
    it "separates the script's arguments from npm's own" $ do
      makeRunScriptCommand Npm "wasp" ["db", "migrate-dev"] `shouldBe` "npm run wasp -- db migrate-dev"
      makeRunScriptCommand Npm "dev" [] `shouldBe` "npm run dev"

    it "passes the arguments as they are with the other package managers" $ do
      makeRunScriptCommand Pnpm "wasp" ["db", "migrate-dev"] `shouldBe` "pnpm run wasp db migrate-dev"
      makeRunScriptCommand Bun "wasp" ["db", "migrate-dev"] `shouldBe` "bun run wasp db migrate-dev"
