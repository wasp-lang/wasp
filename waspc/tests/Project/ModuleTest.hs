module Project.ModuleTest where

import Test.Hspec
import Wasp.Project.Module (packageNameToDirName)

spec_ModuleTest :: Spec
spec_ModuleTest = do
  describe "packageNameToDirName" $ do
    it "keeps unscoped package names" $ do
      packageNameToDirName "fsm" `shouldBe` "fsm"

    it "includes npm package scope in generated directory names" $ do
      packageNameToDirName "@acme/fsm" `shouldBe` "acme-fsm"
