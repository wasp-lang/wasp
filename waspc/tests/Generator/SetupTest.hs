module Generator.SetupTest where

import Data.List (isSubsequenceOf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Wasp.Generator.Setup (SetupGoal (..), SetupStep (..), allSetupSteps, prerequisites, setupStepsFor)

spec_setupStepsFor :: Spec
spec_setupStepsFor =
  describe "setupStepsFor" $ do
    it "prepares the Prisma CLI" $
      setupStepsFor PrismaCliReady `shouldBe` [InstallNpmDeps, FormatPrismaSchema]
    it "builds the SDK and the Prisma client it imports" $
      setupStepsFor SdkReady `shouldBe` [InstallNpmDeps, FormatPrismaSchema, GeneratePrismaClient, BuildSdk]
    it "runs every step for the whole generated app" $
      setupStepsFor GeneratedAppReady `shouldBe` allSetupSteps
    it "includes the steps of every goal before it" $
      zip [minBound ..] (tail [minBound ..])
        `shouldSatisfy` all
          (\(goal, nextGoal) -> setupStepsFor goal `isSubsequenceOf` setupStepsFor nextGoal)

spec_prerequisites :: Spec
spec_prerequisites =
  describe "prerequisites" $ do
    it "are declared before the step that needs them" $
      allSetupSteps
        `shouldSatisfy` all (\step -> all ((< fromEnum step) . fromEnum) (prerequisites step))
