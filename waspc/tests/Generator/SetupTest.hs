module Generator.SetupTest where

import Data.List (inits, nub)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Wasp.Generator.Setup

spec_resolveSetupSteps :: Spec
spec_resolveSetupSteps =
  describe "resolveSetupSteps" $ do
    it "prepares the Prisma CLI" $
      resolveSetupSteps PrismaCliReady `shouldBe` [InstallNpmDeps, FormatPrismaSchema]
    it "builds the SDK and the Prisma client it imports" $
      resolveSetupSteps SdkReady `shouldBe` [InstallNpmDeps, FormatPrismaSchema, GeneratePrismaClient, BuildSdk]
    it "runs every step for the whole generated app" $
      resolveSetupSteps GeneratedAppReady `shouldBe` allSetupSteps
    it "runs each step after its prerequisites" $
      [PrismaCliReady, SdkReady, GeneratedAppReady]
        `shouldSatisfy` all (runsEachStepAfterItsPrerequisites . resolveSetupSteps)
  where
    runsEachStepAfterItsPrerequisites steps =
      and
        [ all (`elem` stepsBefore) (prerequisites step)
        | (step, stepsBefore) <- zip steps (inits steps)
        ]

spec_prerequisites :: Spec
spec_prerequisites =
  describe "prerequisites" $ do
    it "have no cycles" $
      allSetupSteps `shouldSatisfy` all (\step -> step `notElem` transitivePrerequisites step)
  where
    -- Expands the prerequisites a bounded number of times, so a cycle fails
    -- the test instead of looping forever.
    transitivePrerequisites step =
      iterate (\steps -> nub $ steps ++ concatMap prerequisites steps) (prerequisites step)
        !! length allSetupSteps
