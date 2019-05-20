module Generator.PageGeneratorTest where

import Test.Tasty.Hspec

import System.FilePath ((</>))

import Wasp
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.PageGenerator


spec_PageGenerator :: Spec
spec_PageGenerator = do
    let testApp = (App "TestApp" "Test App")
    let testPage = (Page "TestPage" "/test-page" "<div>Test Page</div>")
    let testWasp = (fromApp testApp) `addPage` testPage

    describe "generatePage" $ do
        it "Given a simple Wasp, creates template file draft from _Page.js" $ do
            let (FileDraftTemplateFd (TemplateFileDraft _ srcPath _))
                    = generatePage testWasp (head $ getPages testWasp)
            srcPath `shouldBe` "src" </> "_Page.js"
