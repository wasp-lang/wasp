module Generator.WebAppGenerator.EntityGenerator.EntityFormGeneratorTest where

import Test.Tasty.Hspec

import Path ((</>))

import Wasp
import Generator.FileDraft
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import Generator.WebAppGenerator.EntityGenerator.EntityFormGenerator
import qualified Generator.WebAppGenerator.Common as Common

import qualified Fixtures as F

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

        let expectedDstPath = Common.webAppSrcDirInProjectRootDir
                              </> (entityCreateFormPathInSrc F.taskEntity F.taskCreateForm)

        TmplFD._dstPath templateFileDraft `shouldBe` expectedDstPath
