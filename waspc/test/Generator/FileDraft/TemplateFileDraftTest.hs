module Generator.FileDraft.TemplateFileDraftTest where

import Test.Tasty.Hspec

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Path
import Path ((</>), absdir, relfile)

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
                `shouldBe` [(True, Path.toFilePath $ Path.parent expectedDstPath)]
            Mock.writeFileFromText_calls mockLogs
                `shouldBe` [(Path.toFilePath expectedDstPath, mockTemplateContent)]
              where
                (dstDir, dstPath, templatePath) = ([absdir|/a/b|], [relfile|c/d/dst.txt|], [relfile|e/tmpl.txt|])
                templateData = object [ "foo" .= ("bar" :: String) ]
                fileDraft = createTemplateFileDraft dstPath templatePath (Just templateData)
                expectedDstPath = dstDir </> dstPath
                mockTemplatesDirAbsPath = [absdir|/mock/templates/dir|]
                mockTemplateContent = "Mock template content" :: Text
                mockConfig = Mock.defaultMockConfig
                    { Mock.getTemplatesDirAbsPath_impl = mockTemplatesDirAbsPath
                    , Mock.compileAndRenderTemplate_impl = \_ _ -> mockTemplateContent
                    }
