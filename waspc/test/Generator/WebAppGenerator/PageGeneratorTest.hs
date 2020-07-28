module Generator.WebAppGenerator.PageGeneratorTest where

import Test.Tasty.Hspec

import qualified Path as P

import qualified StrongPath as SP
import Wasp
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.WebAppGenerator.PageGenerator


spec_PageGenerator :: Spec
spec_PageGenerator = do
    let testApp = (App "TestApp" "Test App")
    let testPage = (Page "TestPage" "/test-page" "<div>Test Page</div>" Nothing)
    let testWasp = (fromApp testApp) `addPage` testPage

    describe "generatePageComponent" $ do
        it "Given a simple Wasp, creates template file draft from _Page.js" $ do
            let (FileDraftTemplateFd (TemplateFileDraft _ srcPath _))
                    = generatePageComponent testWasp (head $ getPages testWasp)
            srcPath `shouldBe` SP.fromPathRelFile [P.relfile|react-app/src/_Page.js|]
