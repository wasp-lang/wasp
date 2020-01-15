module Generator.FileDraft.TemplateFileDraftTest where

import Test.Tasty.Hspec

import Data.Aeson (object, (.=))
import System.FilePath ((</>), takeDirectory)
import Data.Text (Text)

import Generator.FileDraft

import qualified Generator.MockWriteableMonad as Mock


spec_TemplateFileDraft :: Spec
spec_TemplateFileDraft = do
    describe "write" $ do
        it "Creates new file from existing template file" $ do
            let mock = write dstDir fileDraft
            let mockLogs = Mock.getMockLogs mock mockConfig
            Mock.compileAndRenderTemplate_calls mockLogs
                `shouldBe` [(templatePath, templateData)]
            Mock.createDirectoryIfMissing_calls mockLogs
                `shouldBe` [(True, takeDirectory expectedDstPath)]
            Mock.writeFileFromText_calls mockLogs
                `shouldBe` [(expectedDstPath, mockTemplateContent)]
              where
                (dstDir, dstPath, templatePath) = ("a/b", "c/d/dst.txt", "e/tmpl.txt")
                templateData = object [ "foo" .= ("bar" :: String) ]
                fileDraft = createTemplateFileDraft dstPath templatePath templateData
                expectedDstPath = dstDir </> dstPath
                mockTemplatesDirAbsPath = "mock/templates/dir"
                mockTemplateContent = "Mock template content" :: Text
                mockConfig = Mock.defaultMockConfig
                    { Mock.getTemplatesDirAbsPath_impl = mockTemplatesDirAbsPath
                    , Mock.compileAndRenderTemplate_impl = \_ _ -> mockTemplateContent
                    }
