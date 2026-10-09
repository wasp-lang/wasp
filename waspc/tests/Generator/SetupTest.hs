module Generator.SetupTest where

import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Generator.Setup (SetupStep (..), allSetupSteps, deduplicateAndOrderSetupSteps)

spec_deduplicateAndOrderSetupSteps :: Spec
spec_deduplicateAndOrderSetupSteps =
  describe "deduplicateAndOrderSetupSteps" $ do
    it "runs the steps in declaration order no matter what order they were requested in" $
      deduplicateAndOrderSetupSteps (reverse allSetupSteps) `shouldBe` allSetupSteps
    it "runs only the requested steps" $
      deduplicateAndOrderSetupSteps [InstallNpmDeps, BuildSdk] `shouldBe` [InstallNpmDeps, BuildSdk]
    it "runs a step once even when it is requested twice" $
      deduplicateAndOrderSetupSteps [BuildSdk, BuildSdk] `shouldBe` [BuildSdk]
    it "runs nothing when nothing is requested" $
      deduplicateAndOrderSetupSteps [] `shouldBe` []
