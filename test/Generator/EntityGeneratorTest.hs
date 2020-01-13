module Generator.EntityGeneratorTest where

import Test.Tasty.Hspec

import System.FilePath ((</>))

import Wasp
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.EntityGenerator
import qualified Generator.Common as Common

import qualified Fixtures as F


spec_EntityGenerator :: Spec
spec_EntityGenerator = do
    let testApp = (App "TestApp" "Test App")
    let testEntity = (Entity "TestEntity" [EntityField "testField" EftString])
    let testWasp = (fromApp testApp) `addEntity` testEntity

    describe "generateEntityClass" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityClass (entityTemplatesDirPath </> "_Entity.js")

    describe "generateEntityActions" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityActions (entityTemplatesDirPath </> "actions.js")

    describe "generateEntityActionTypes" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityActionTypes (entityTemplatesDirPath </> "actionTypes.js")

    describe "generateEntityState" $ do
        testGeneratorUsesCorrectSrcPath
            testWasp generateEntityState (entityTemplatesDirPath </> "state.js")

  where
      testGeneratorUsesCorrectSrcPath testWasp generator expectedSrcPath = it
          "Given a simple Wasp, creates template file draft with correct src path" $ do
              let (FileDraftTemplateFd (TemplateFileDraft _ srcPath _))
                      = generator testWasp (head $ getEntities testWasp)
              srcPath `shouldBe` expectedSrcPath


-- TODO(matija): is it ok to have this separated like this? Maybe it would make more
-- sense to have all under one roof? But still we want to localize dummy data needed by tests?
spec_generateEntityCreateForm :: Spec
spec_generateEntityCreateForm = do
    let waspWithTask = fromWaspElems
            [ WaspElementApp F.app
            , WaspElementEntity F.taskEntity
            , WaspElementEntityForm F.taskCreateForm
            ]

    {-
    -- NOTE(matija): Could not get this to work - the problem was that FileDraft has to be the
    -- instance of NFData typeclass so it can be fully evaulated, I did not want to go further
    -- into that.
    it "When given entity form for which there is no entity in wasp, throws an error." $ do
        (evaluate . force) (generateEntityCreateForm waspWithTask F.userCreateForm) 
            `shouldThrow`
        anyErrorCall
    -}

    it "When given entity form for which there is an entity in wasp, \
        \returns correct file draft." $ do
        let (FileDraftTemplateFd templateFileDraft) =
                generateEntityCreateForm waspWithTask F.taskCreateForm

        let expectedDstPath = Common.srcDirPath </> (entityCreateFormPathInSrc F.taskEntity F.taskCreateForm)

        templateFileDraftDstFilepath templateFileDraft `shouldBe` expectedDstPath
