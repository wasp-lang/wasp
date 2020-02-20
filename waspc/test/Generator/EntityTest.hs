module Generator.EntityTest where

import Test.Tasty.Hspec

import Path ((</>), relfile)

import Wasp
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.Entity


spec_EntityGenerator :: Spec
spec_EntityGenerator = do
    let testApp = (App "TestApp" "Test App")
    let testEntity = (Entity "TestEntity" [EntityField "testField" EftString])
    let testWasp = (fromApp testApp) `addEntity` testEntity

    describe "generateEntityClass" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityClass (entityTemplatesDirPath </> [relfile|_Entity.js|])

    describe "generateEntityActions" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityActions (entityTemplatesDirPath </> [relfile|actions.js|])

    describe "generateEntityActionTypes" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityActionTypes (entityTemplatesDirPath </> [relfile|actionTypes.js|])

    describe "generateEntityState" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityState (entityTemplatesDirPath </> [relfile|state.js|])

  where
      testGeneratorUsesCorrectSrcPath testWasp generator expectedSrcPath = it
          "Given a simple Wasp, creates template file draft with correct src path" $ do
              let (FileDraftTemplateFd (TemplateFileDraft _ srcPath _))
                      = generator testWasp (head $ getEntities testWasp)
              srcPath `shouldBe` expectedSrcPath
