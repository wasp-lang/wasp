module Generator.SetupTest where

import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Generator.Setup (SetupStep (..), allSetupSteps, orderSetupSteps)

spec_orderSetupSteps :: Spec
spec_orderSetupSteps =
  describe "orderSetupSteps" $ do
    it "runs the steps in declaration order whatever order they were requested in" $
      orderSetupSteps (reverse allSetupSteps) `shouldBe` allSetupSteps
    it "runs only the requested steps" $
      orderSetupSteps [BuildSdk, InstallNpmDeps] `shouldBe` [InstallNpmDeps, BuildSdk]
    it "runs a step once even when it is requested twice" $
      orderSetupSteps [BuildSdk, BuildSdk] `shouldBe` [BuildSdk]
    it "runs nothing when nothing is requested" $
      orderSetupSteps [] `shouldBe` []
